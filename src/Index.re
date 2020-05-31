// Entry point

[@bs.val] external document: Js.t({..}) = "document";

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
  // let (scene, id3) =
  //   scene->Point.circle(
  //     ~sym=Some({center: Ref.id(id2), count: 6}),
  //     Ref.id(id1),
  //     Ref.id(id2),
  //     Js.Math._PI /. 3. *. 2.,
  //     1.0,
  //   );
  // let (scene, id4) = scene->Point.line(Ref.id(id1), Ref.id(id3), 0.5);
  // let (scene, id5) = scene->Point.line(Ref.id(id2), Ref.id(id1), 2.);

  // let (scene, _) =
  //   scene->Shape.line(
  //     ~sym=Some({center: Ref.id(id2), count: 6}),
  //     Ref.id(id5),
  //     Ref.sym(id1, 5),
  //   );

  // let (scene, _) =
  //   scene->Shape.circle(
  //     ~sym=Some({center: Ref.id(id2), count: 6}),
  //     Ref.id(id1),
  //     Ref.id(id3),
  //   );

  // let (scene, _) =
  //   scene->Shape.circle(
  //     ~sym=Some({center: Ref.id(id2), count: 6}),
  //     Ref.id(id4),
  //     Ref.id(id1),
  //   );

  scene;
};

module App = {
  type state = {
    hover: option([ | `Point(Types.reference) | `Shape(Types.reference)]),
    showPoints: bool,
    showTraces: bool,
    scene: Types.scene,
    selection: option(Types.selection),
    history: list(Types.scene),
  };
  let initial = {
    hover: None,
    showPoints: true,
    showTraces: true,
    scene: Controls.getInitial(scene),
    selection: None,
    history: [],
  };

  let reduce = (state, action) => {
    switch (action) {
    | `SetHover(hover) => {...state, hover}
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
  let make = () => {
    let (transform, setTransform) =
      Hooks.useState({Canvas.zoom: 2., dx: 0., dy: 0.});
    let (state, dispatch) = React.useReducer(reduce, initial);
    // let (scene, updateScene) = React.useState(() => scene);
    // let (selection, setSelection) = React.useState(() => None);
    <div className=Css.(style([display(`flex), flexDirection(`row)]))>
      <div>
        <Canvas
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
      </div>
      <div>
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
      <Gallery current="" onLoad={id => {Js.log(id)}} />
    </div>;
  };
};

ReactDOMRe.render(<App />, document##getElementById("root"));