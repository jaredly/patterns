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
  [@react.component]
  let make = (~shape) => {
    switch (shape) {
    | CLine({p1, p2}) =>
      // let (x1, y1, x2, y2) = Calculate.line(p1, p2, scene, points);
      <line
        x1={s(p1.x)}
        y1={s(p1.y)}
        x2={s(p2.x)}
        y2={s(p2.y)}
        strokeWidth="1"
        stroke="red"
      />
    | CCircle({center, r}) =>
      // let (cx, cy, r) = Calculate.circle(center, onEdge, scene, points);
      <circle
        cx={s(center.x)}
        cy={s(center.y)}
        r={s(r)}
        fill="none"
        strokeWidth="1"
        stroke="red"
      />
    };
  };
};

let tblList = tbl => {
  Hashtbl.fold((k, v, l) => [(k, v), ...l], tbl, []);
};

[@react.component]
let make = (~scene: scene) => {
  let (points, symPoints, shapes, symShapes) =
    React.useMemo1(
      () => {Calculate.calculateAllPositions(scene)},
      [|scene|],
    );
  <svg width="500px" height="500px">
    {tblList(points)
     ->Belt.List.toArray
     ->Belt.Array.map(((id, {x, y})) => {
         <circle
           key=id
           cx={Js.Float.toString(x)}
           cy={Js.Float.toString(y)}
           r="4"
           fill="red"
         />
       })
     ->React.array}
    {symPoints
     ->Belt.Array.mapWithIndex((i, (id, {x, y})) => {
         <circle
           key={string_of_int(i)}
           cx={Js.Float.toString(x)}
           cy={Js.Float.toString(y)}
           r="4"
           fill="rgba(0,0,255,0.5"
         />
       })
     ->React.array}
    {shapes
     ->Belt.Array.map(((k, shape)) => <Shape key=k shape />)
     ->React.array}
    {symShapes
     ->Belt.Array.mapWithIndex((i, (k, shape)) =>
         <Shape key={string_of_int(i)} shape />
       )
     ->React.array}
  </svg>;
  // {scene.shapes
  //  ->Belt.Map.String.toArray
  //  ->Belt.Array.map(((k, shape)) => {<Shape key=k shape scene points />})
  //  ->React.array}
};