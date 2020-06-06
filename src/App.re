let scene = {
  open Api;
  open Types;
  let scene = init();
  let (scene, id2) = scene->Point.abs(0., 0.);
  let (scene, _id1) =
    scene->Point.abs(~sym=Some({center: Ref.id(id2), count: 5}), 0., -200.);

  scene;
};

type state = {
  id: option(string),
  hover: option(Types.hover),
  scene: Types.scene,
  svgRef: React.ref(Js.Nullable.t(Dom.element)),
  selection: Types.selection,
  history: list(Types.scene),
};

let loadInitial = () => {
  Controls.getInitial(scene)
  |> Js.Promise.then_(((id, scene)) => {
       Js.Promise.resolve({
         id,
         hover: None,
         svgRef: {
           current: Js.Nullable.null,
         },
         scene,
         selection: Types.emptySelection,
         history: [],
       })
     });
};

let reduce = (state, action) => {
  switch (action) {
  | `SetHover(hover) => {...state, hover}
  | `Load(id, scene) => {...state, id: Some(id), scene}
  | `Undo =>
    switch (state.history) {
    | [] => state
    | [scene, ...history] => {
        ...state,
        scene,
        history,
        selection: Types.emptySelection,
      }
    }
  | `TogglePoints => {
      ...state,
      scene: {
        ...state.scene,
        presentation: {
          ...state.scene.presentation,
          points: !state.scene.presentation.points,
        },
      },
    }
  | `ToggleTraces => {
      ...state,
      scene: {
        ...state.scene,
        presentation: {
          ...state.scene.presentation,
          traces: !state.scene.presentation.traces,
        },
      },
    }
  | `DeselectTile(id) => {
      ...state,
      selection: {
        ...state.selection,
        tiles: state.selection.tiles->Belt.List.keep(k => k.id != id),
      },
    }
  | `SelectTile(reference) => {
      ...state,
      selection: {
        ...state.selection,
        tiles:
          if (state.selection.tiles->Belt.List.has(reference, (==))) {
            state.selection.tiles->Belt.List.keep(k => k != reference);
          } else {
            [reference, ...state.selection.tiles];
          },
      },
    }
  | `SelectShape(reference) => {
      ...state,
      selection: {
        ...state.selection,
        shapes:
          if (state.selection.shapes->Belt.List.has(reference, (==))) {
            state.selection.shapes->Belt.List.keep(k => k != reference);
          } else {
            [reference, ...state.selection.shapes];
          },
      },
    }
  | `SelectPoint(reference) => {
      ...state,
      selection: {
        ...state.selection,
        points:
          if (state.selection.points->Belt.List.has(reference, (==))) {
            state.selection.points->Belt.List.keep(k => k != reference);
          } else {
            [reference, ...state.selection.points];
          },
      },
    }
  | `SetSelection(selection) => {...state, selection}
  | `SetScene(scene) => {
      ...state,
      scene,
      history: [state.scene, ...state.history],
    }
  | `SetColor(s, color) => {
      ...state,
      scene: state.scene->Api.Shape.setColor(s.Types.id, color),
    }
  };
};

[@react.component]
let make = (~initial) => {
  let width = 800;
  let height = 800;

  let (state, dispatch) = React.useReducer(reduce, initial);

  <div className=Css.(style([display(`flex), flexDirection(`row)]))>
    <div>
      <Canvas
        width
        height
        innerRef={state.svgRef}
        scene={state.scene}
        hover={state.hover}
        selection={state.selection}
        selectPoint={res => dispatch(`SelectPoint(res))}
        selectShape={res => dispatch(`SelectShape(res))}
        selectTile={res => dispatch(`SelectTile(res))}
        setScene={s => dispatch(`SetScene(s))}
        setSelection={s => dispatch(`SetSelection(s))}
      />
      <Controls
        selection={state.selection}
        setSelection={s => dispatch(`SetSelection(s))}
        scene={state.scene}
        setScene={s => dispatch(`SetScene(s))}
        togglePoints={() => dispatch(`TogglePoints)}
        toggleTraces={() => dispatch(`ToggleTraces)}
        setColor={(s, color) => dispatch(`SetColor((s, color)))}
        onUndo={() => dispatch(`Undo)}
      />
      <Gallery
        current={state.id}
        onLoad={id => {
          Gallery.loadState(id)
          |> Js.Promise.then_(scene => {
               switch (scene) {
               | None => ()
               | Some(scene) =>
                 Controls.Location.setHash(id);
                 dispatch(`Load((id, Versions.upgrade(scene))));
               };
               Js.Promise.resolve();
             })
          |> ignore
        }}
        onSave={() => {
          let id =
            switch (state.id) {
            | None =>
              let id = Api.genId();
              Controls.Location.setHash(id);
              id;
            | Some(id) => id
            };
          Gallery.saveState(id, state.scene)->ignore;
          let blob: Gallery.blob = [%bs.raw
            "new Blob([state.svgRef.current.outerHTML], {type: 'image/svg+xml'})"
          ];
          Gallery.saveScreenshot(id, blob)->ignore;
          (id, blob);
        }}
      />
    </div>
    <div>
      {React.string("Zoom")}
      <input
        value={state.scene.presentation.transform.zoom->Js.Float.toString}
        onChange={evt => {
          let zoom = evt->ReactEvent.Form.target##value->Js.Float.fromString;
          dispatch(
            `SetScene({
              ...state.scene,
              presentation: {
                ...state.scene.presentation,
                transform: {
                  ...state.scene.presentation.transform,
                  zoom,
                },
              },
            }),
          );
        }}
      />
      <Sidebar
        scene={state.scene}
        selection={state.selection}
        selectPoint={res => dispatch(`SelectPoint(res))}
        selectShape={res => dispatch(`SelectShape(res))}
        selectTile={res => dispatch(`SelectTile(res))}
        deselectTile={res => dispatch(`DeselectTile(res))}
        setHovered={s => dispatch(`SetHover(s))}
        setScene={s => dispatch(`SetScene(s))}
        setSelection={s => dispatch(`SetSelection(s))}
      />
    </div>
  </div>;
};