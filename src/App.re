let scene = {
  open Api;
  open Types;
  let scene = init();
  let (scene, id2) = scene->Point.abs(250., 250.);
  let (scene, _id1) =
    scene->Point.abs(
      ~sym=Some({center: Ref.id(id2), count: 5}),
      200.,
      200.,
    );

  scene;
};

type state = {
  id: option(string),
  hover: option([ | `Point(Types.reference) | `Shape(Types.reference)]),
  showPoints: bool,
  showTraces: bool,
  scene: Types.scene,
  svgRef: React.ref(Js.Nullable.t(Dom.element)),
  selection: option(Types.selection),
  history: list(Types.scene),
};

// let (id, scene) = Controls.getInitial(scene);
let loadInitial = () => {
  Controls.getInitial(scene)
  |> Js.Promise.then_(((id, scene)) => {
       Js.Promise.resolve({
         id,
         hover: None,
         showPoints: true,
         showTraces: true,
         svgRef: {
           current: Js.Nullable.null,
         },
         scene,
         selection: None,
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
    | [scene, ...history] => {...state, scene, history, selection: None}
    }
  | `TogglePoints => {...state, showPoints: !state.showPoints}
  | `ToggleTraces => {...state, showTraces: !state.showTraces}
  | `SelectShape(reference) => {
      ...state,
      selection:
        Some(
          Shapes(
            switch (state.selection) {
            | Some(Shapes(p)) =>
              if (p->Belt.List.has(reference, (==))) {
                p->Belt.List.keep(k => k != reference);
              } else {
                [reference, ...p];
              }
            | _ => [reference]
            },
          ),
        ),
    }
  | `SelectPoint(reference) => {
      ...state,
      selection:
        switch (state.selection) {
        | Some(Points(p)) =>
          if (p->Belt.List.has(reference, (==))) {
            Some(Points(p->Belt.List.keep(k => k != reference)));
          } else {
            Some(Points([reference, ...p]));
          }
        | _ => Some(Points([reference]))
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
  let width = 1000;
  let height = 1000;

  let hw = float_of_int(width) /. 2.0;
  let hh = float_of_int(height) /. 2.0;
  let (transform, setTransform) =
    Hooks.useState({Canvas.zoom: 2., dx: 0., dy: 0.});

  let (state, dispatch) = React.useReducer(reduce, initial);

  <div className=Css.(style([display(`flex), flexDirection(`row)]))>
    <div>
      <Canvas
        width
        height
        innerRef={state.svgRef}
        transform
        showPoints={state.showPoints}
        showTraces={state.showTraces}
        scene={state.scene}
        hover={state.hover}
        selection={state.selection}
        selectPoint={res => dispatch(`SelectPoint(res))}
        selectShape={res => dispatch(`SelectShape(res))}
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
                 dispatch(`Load((id, scene)));
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
      <button
        onClick={_ => {
          let nzoom = transform.zoom *. 1.5;
          let cx = hw /. transform.zoom +. transform.dx;
          let cy = hh /. transform.zoom +. transform.dy;
          let ndx = cx -. hw /. nzoom;
          let ndy = cy -. hh /. nzoom;
          Js.log3("ok", cx, cy);
          setTransform({zoom: nzoom, dx: ndx, dy: ndy});
        }}>
        {React.string("+")}
      </button>
      <button
        onClick={_ => {
          let nzoom = transform.zoom /. 1.5;
          let cx = hw /. transform.zoom +. transform.dx;
          let cy = hh /. transform.zoom +. transform.dy;
          let ndx = cx -. hw /. nzoom;
          let ndy = cy -. hh /. nzoom;
          setTransform({zoom: nzoom, dx: ndx, dy: ndy});
        }}>
        {React.string("-")}
      </button>
      {React.string("Zoom")}
      <input
        value={transform.zoom->Js.Float.toString}
        onChange={evt => {
          let zoom = evt->ReactEvent.Form.target##value->Js.Float.fromString;
          setTransform({...transform, zoom});
        }}
      />
      {React.string("dx")}
      <input
        value={transform.dx->Js.Float.toString}
        onChange={evt => {
          let dx = evt->ReactEvent.Form.target##value->Js.Float.fromString;
          setTransform({...transform, dx});
        }}
      />
      {React.string("dy")}
      <input
        value={transform.dy->Js.Float.toString}
        onChange={evt => {
          let dy = evt->ReactEvent.Form.target##value->Js.Float.fromString;
          setTransform({...transform, dy});
        }}
      />
      <Sidebar
        scene={state.scene}
        selection={state.selection}
        selectPoint={res => dispatch(`SelectPoint(res))}
        selectShape={res => dispatch(`SelectShape(res))}
        setHovered={s => dispatch(`SetHover(s))}
        setScene={s => dispatch(`SetScene(s))}
        setSelection={s => dispatch(`SetSelection(s))}
      />
    </div>
  </div>;
};