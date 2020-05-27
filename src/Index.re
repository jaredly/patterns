// Entry point

[@bs.val] external document: Js.t({..}) = "document";

let scene = {
  open Api;
  open Types;
  let scene = init();
  let (scene, id2) = scene->Point.abs(250., 250.);
  let (scene, id1) =
    scene->Point.abs(
      ~sym=Some({center: Ref.id(id2), count: 6}),
      170.,
      170.,
    );
  let (scene, id3) =
    scene->Point.circle(
      ~sym=Some({center: Ref.id(id2), count: 6}),
      Ref.id(id1),
      Ref.id(id2),
      Js.Math._PI /. 3. *. 2.,
      1.0,
    );
  let (scene, id4) = scene->Point.line(Ref.id(id1), Ref.id(id3), 0.5);
  let (scene, id5) = scene->Point.line(Ref.id(id2), Ref.id(id1), 2.);

  let (scene, _) =
    scene->Shape.line(
      ~sym=Some({center: Ref.id(id2), count: 6}),
      Ref.id(id5),
      Ref.sym(id1, 5),
    );

  let (scene, _) =
    scene->Shape.circle(
      ~sym=Some({center: Ref.id(id2), count: 6}),
      Ref.id(id1),
      Ref.id(id3),
    );

  let (scene, _) =
    scene->Shape.circle(
      ~sym=Some({center: Ref.id(id2), count: 6}),
      Ref.id(id4),
      Ref.id(id1),
    );

  scene;
};

module App = {
  type state = {
    scene: Types.scene,
    selection: option(Types.selection),
    history: list(Types.scene),
  };
  let initial = {scene, selection: None, history: []};

  let reduce = (state, action) => {
    switch (action) {
    | `Undo =>
      switch (state.history) {
      | [] => state
      | [scene, ...history] => {scene, history, selection: None}
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
    };
  };

  [@react.component]
  let make = () => {
    let (state, dispatch) = React.useReducer(reduce, initial);
    // let (scene, updateScene) = React.useState(() => scene);
    // let (selection, setSelection) = React.useState(() => None);
    <div>
      <Canvas
        scene={state.scene}
        selection={state.selection}
        selectPoint={res => dispatch(`SelectPoint(res))}
      />
      <Controls
        selection={state.selection}
        setSelection={s => dispatch(`SetSelection(s))}
        scene={state.scene}
        setScene={s => dispatch(`SetScene(s))}
        onUndo={() => dispatch(`Undo)}
      />
    </div>;
  };
};

ReactDOMRe.render(<App />, document##getElementById("root"));