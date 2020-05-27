open Types;
open Api;

let buttonsForPoints = (points, scene, setSelection, setScene, onUndo) => {
  let clear = ("Clear", () => setSelection(None));
  switch (points) {
  | [_] => [clear]
  | [p1, p2] => [
      clear,
      (
        "Add midpoint",
        (
          () => {
            let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
            // setSelection(None);
            let (scene, id) = scene->Api.Point.line(~sym, p1, p2, 0.5);
            setScene(scene);
            setSelection(Some(Points([{id, index: 0}])));
          }
        ),
      ),
      (
        "Add circle",
        (
          () => {
            let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
            let (scene, id) = scene->Api.Shape.circle(~sym, p1, p2);
            setScene(scene);
            setSelection(Some(Shape([{id, index: 0}])));
          }
        ),
      ),
      (
        "Add line",
        (
          () => {
            let {pos: _, sym} = Belt.Map.String.getExn(scene.points, p1.id);
            let (scene, id) = scene->Api.Shape.line(~sym, p1, p2);
            setScene(scene);
            setSelection(Some(Shape([{id, index: 0}])));
          }
        ),
      ),
    ]
  | _ => [clear]
  };
};

[@react.component]
let make = (~selection, ~setSelection, ~scene, ~setScene, ~onUndo) => {
  let buttons = [
    ("Undo", onUndo),
    ...switch (selection) {
       | Some(Points(items)) =>
         buttonsForPoints(items, scene, setSelection, setScene, onUndo)
       | _ => []
       },
  ];
  <div>
    {buttons
     ->Array.of_list
     ->Belt.Array.map(((title, action)) =>
         <button key=title onClick={_ => action()}>
           {React.string(title)}
         </button>
       )
     ->React.array}
  </div>;
};