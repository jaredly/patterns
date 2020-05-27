open Types;

// module Shape = {
//     [@react.component]
//     let make = (~shape: shape) => {
//         switch (shape.kind) {
//             | Line({p1, p2}) => <Line p1 p2 />
//             | Circle({pos, radius}) => <Circle pos radius />
//             | Arc({p1, p2, p3}) => <Arc p1 p2 p3 />
//         }
//     }
// }

let s = Js.Float.toString;

module Shape = {
  module Inner = {
    [@react.component]
    let make = (~shape, ~onClick, ~style, ~strokeWidth, ~stroke) => {
      switch (shape) {
      | CLine({p1, p2}) =>
        <line
          onClick
          x1={s(p1.x)}
          y1={s(p1.y)}
          x2={s(p2.x)}
          y2={s(p2.y)}
          style
          strokeWidth
          stroke
        />
      | CCircle({center, r}) =>
        <circle
          onClick
          cx={s(center.x)}
          cy={s(center.y)}
          r={s(r)}
          fill="none"
          strokeWidth
          style
          stroke
        />
      };
    };
  };

  [@react.component]
  let make = (~shape, ~color, ~isSelected, ~onSelect) => {
    <Inner
      shape
      onClick={_ => onSelect()}
      style={ReactDOMRe.Style.make(~cursor="pointer", ())}
      strokeWidth={isSelected ? "4" : "2"}
      stroke={
        switch (color) {
        | None => "#eee"
        | Some(c) => c
        }
      }
    />;
  };
};

let tblList = tbl => {
  Hashtbl.fold((k, v, l) => [(k, v), ...l], tbl, []);
};

let toId = ({id, index}) => id ++ "_" ++ string_of_int(index);

let isShapeSelected = (selection, r) =>
  switch (selection) {
  | Some(Shapes(shapes)) => shapes->Belt.List.has(r, (==))
  | _ => false
  };

let isSelected = (selection, r) =>
  switch (selection) {
  | Some(Points(points)) => points->Belt.List.has(r, (==))
  | _ => false
  };

[@react.component]
let make =
    (
      ~scene: scene,
      ~selection: option(selection),
      ~selectPoint: reference => unit,
      ~selectShape: reference => unit,
      ~showPoints,
    ) => {
  let (_positions, points, shapes) =
    React.useMemo1(
      () => {Calculate.calculateAllPositions(scene)},
      [|scene|],
    );
  <svg width="500px" height="500px">
    {shapes
     ->Belt.Array.map(((k, shape, color)) =>
         <Shape
           color
           isSelected={isShapeSelected(selection, k)}
           onSelect={() => selectShape(k)}
           key={toId(k)}
           shape
         />
       )
     ->React.array}
    {showPoints
       ? points
         ->Belt.Array.map(((k, {x, y})) => {
             <circle
               key={toId(k)}
               cx={Js.Float.toString(x)}
               cy={Js.Float.toString(y)}
               onClick={_ => selectPoint(k)}
               r="4"
               fill={k.index == 0 ? "red" : "rgba(0,0,255,0.2)"}
               stroke="black"
               strokeWidth={isSelected(selection, k) ? "3" : "0"}
               style={ReactDOMRe.Style.make(~cursor="pointer", ())}
             />
           })
         ->React.array
       : React.null}
  </svg>;
};