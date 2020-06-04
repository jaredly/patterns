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
  // showPoints: bool,
  // showTraces: bool,
  scene: Types.scene,
  svgRef: React.ref(Js.Nullable.t(Dom.element)),
  selection: Types.selection,
  history: list(Types.scene),
};

let emptySelection: Types.selection = {tiles: [], shapes: [], points: []};

// let (id, scene) = Controls.getInitial(scene);
let loadInitial = () => {
  Controls.getInitial(scene)
  |> Js.Promise.then_(((id, scene)) => {
       Js.Promise.resolve({
         id,
         hover: None,
         //  showPoints: true,
         //  showTraces: true,
         svgRef: {
           current: Js.Nullable.null,
         },
         scene,
         selection: emptySelection,
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
        selection: emptySelection,
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
                 Controls.Location.setHash(Controls.Location.location, id);
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
              Controls.Location.setHash(Controls.Location.location, id);
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
        // setTransform({...transform, zoom});
      />
      // {React.string("dx")}
      // <input
      //   value={transform.dx->Js.Float.toString}
      //   onChange={evt => {
      //     let dx = evt->ReactEvent.Form.target##value->Js.Float.fromString;
      //     setTransform({...transform, dx});
      //   }}
      // />
      // {React.string("dy")}
      // <input
      //   value={transform.dy->Js.Float.toString}
      //   onChange={evt => {
      //     let dy = evt->ReactEvent.Form.target##value->Js.Float.fromString;
      //     setTransform({...transform, dy});
      //   }}
      // />
      <Sidebar
        scene={state.scene}
        selection={state.selection}
        selectPoint={res => dispatch(`SelectPoint(res))}
        selectShape={res => dispatch(`SelectShape(res))}
        selectTile={res => dispatch(`SelectTile(res))}
        setHovered={s => dispatch(`SetHover(s))}
        setScene={s => dispatch(`SetScene(s))}
        setSelection={s => dispatch(`SetSelection(s))}
      />
    </div>
  </div>;
  // </button>
  //   {React.string("-")}
  //   }}>
  //     setTransform({zoom: nzoom, dx: ndx, dy: ndy});
  //     let ndy = cy -. hh /. nzoom;
  //     let ndx = cx -. hw /. nzoom;
  //     let cy = hh /. scene.presentation.transform.zoom +. scene.presentation.transform.center.y;
  //     let cx = hw /. scene.presentation.transform.zoom +. scene.presentation.transform.center.x;
  //     let nzoom = scene.presentation.transform.zoom /. 1.5;
  //   onClick={_ => {
  // <button
  // </button>
  //   {React.string("+")}
  //   }}>
  //     setTransform({zoom: nzoom, dx: ndx, dy: ndy});
  //     Js.log3("ok", cx, cy);
  //     // let ndy = cy -. hh /. nzoom;
  //     // let ndx = cx -. hw /. nzoom;
  //     // let cy = hh /. scene.presentation.transform.zoom +. scene.presentation.transform.center.y;
  //     // let cx = hw /. scene.presentation.transform.zoom +. scene.presentation.transform.center.x;
  //     let nzoom = scene.presentation.transform.zoom *. 1.5;
  //   onClick={_ => {
  // <button
};