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

type transform = {
  zoom: float,
  dx: float,
  dy: float,
};

let tx = (x, transform) => (x -. transform.dx) *. transform.zoom;
let ty = (y, transform) => (y -. transform.dy) *. transform.zoom;
let tf = (f, transform) => f *. transform.zoom;

let s = Js.Float.toString;

let normalizeTheta = t => t < 0. ? Js.Math._PI *. 2. +. t : t;

module Shape = {
  // let makePolyPath = (p0, items) => {
  //   [
  //     Printf.sprintf("M %0.2f %0.2f", p0.x, p0.y),
  //     ...items->Belt.List.map(item =>
  //          switch (item) {
  //          | `Line(pos) => Printf.sprintf("L %0.2f %0.2f", pos.x, pos.y)
  //          | `Arc({to_, r, sweep}) =>
  //            Printf.sprintf(
  //              {|A %0.2f %0.2f
  //           0
  //           %d 1
  //           %0.2f %0.2f|},
  //              r,
  //              r,
  //              sweep ? 1 : 0,
  //              to_.x,
  //              to_.y,
  //            )
  //          }
  //        ),
  //   ]
  //   |> String.concat(" ");
  // };

  module Inner = {
    [@react.component]
    let make =
        (
          ~className,
          ~transform,
          ~shape,
          ~onClick,
          ~style,
          ~strokeWidth,
          ~stroke,
        ) => {
      switch (shape) {
      | CLine({p1, p2}) =>
        <line
          onClick
          className
          x1={s(tx(p1.x, transform))}
          y1={s(ty(p1.y, transform))}
          x2={s(tx(p2.x, transform))}
          y2={s(ty(p2.y, transform))}
          style
          strokeWidth
          stroke
        />
      | CCircle({center, r}) =>
        <circle
          onClick
          className
          cx={s(tx(center.x, transform))}
          cy={s(ty(center.y, transform))}
          r={s(tf(r, transform))}
          fill="none"
          strokeWidth
          style
          stroke
        />
      // | CPoly({p0, items}) =>
      //   <path
      //     d={makePolyPath(p0, items)}
      //     onClick
      // className
      //     fill="#afa"
      //     strokeWidth
      //     style
      //     stroke
      //   />
      | CCirclePart({center, r, theta0, theta1}) =>
        let start = {
          x: center.x +. cos(theta0) *. r,
          y: center.y +. sin(theta0) *. r,
        };
        let endd = {
          x: center.x +. cos(theta1) *. r,
          y: center.y +. sin(theta1) *. r,
        };
        <path
          className
          d={Printf.sprintf(
            {|M %0.2f %0.2f
            A %0.2f %0.2f
            0
            %d 1
            %0.2f %0.2f|},
            tx(start.x, transform),
            ty(start.y, transform),
            tf(r, transform),
            tf(r, transform),
            normalizeTheta(theta1 -. theta0) > Js.Math._PI ? 1 : 0,
            tx(endd.x, transform),
            ty(endd.y, transform),
          )}
          onClick
          fill="none"
          strokeWidth
          style
          stroke
        />;
      };
    };
  };

  [@react.component]
  let make = (~transform, ~isHovered, ~shape, ~color, ~isSelected, ~onSelect) => {
    <React.Fragment>

        <Inner
          shape
          transform
          className=Css.(
            isSelected
              ? ""
              : style([
                  hover([unsafe("stroke", "rgba(100, 220, 255, 0.5)")]),
                ])
          )
          onClick={_ => onSelect()}
          style={ReactDOMRe.Style.make(~cursor="pointer", ())}
          strokeWidth="4"
          stroke={
            isSelected
              ? "rgba(0, 255, 0, 0.5)"
              : isHovered ? "rgba(100, 220, 255, 0.25)" : "rgba(0,0,0,0)"
          }
        />
        <Inner
          shape
          transform
          onClick={_ => ()}
          className=Css.(style([pointerEvents(`none)]))
          style={ReactDOMRe.Style.make(~cursor="pointer", ())}
          strokeWidth="1"
          stroke={
            switch (color) {
            | None => "rgba(255, 0, 255, 0.1)"
            | Some(c) => c
            }
          }
        />
      </React.Fragment>;
      // {isSelected
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

let isShapeHovered = (selection, r) =>
  switch (selection) {
  | Some(Shapes(shapes)) => shapes->Belt.List.some(s => s.id == r.id)
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
      ~hover: option(hover),
      ~transform: transform,
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
  <svg width="1000px" height="1000px">
    {shapes
     ->Js.Array2.sortInPlaceWith(((_, _, a), (_, _, b)) =>
         switch (a, b) {
         | (None, Some(_)) => (-1)
         | (Some(_), None) => 1
         | _ => 0
         }
       )
     ->Belt.Array.map(((k, shape, color)) =>
         <Shape
           color
           isHovered={
             (
               switch (hover) {
               | Some(`Shape({id})) when id == k.id => true
               | _ => false
               }
             )
             || isShapeHovered(selection, k)
           }
           transform
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
               cx={Js.Float.toString(tx(x, transform))}
               cy={Js.Float.toString(ty(y, transform))}
               onClick={_ => selectPoint(k)}
               r="2"
               //  fill={k.index == 0 ? "red" : "rgba(0,0,255,0.2)"}
               fill="rgba(255,255,255,0.2)"
               stroke="rgba(0,0,0,0.5)"
               strokeWidth={
                 isSelected(selection, k) || hover == Some(`Point(k))
                   ? "3" : "1"
               }
               //  strokeWidth={isSelected(selection, k) ? "3" : "0"}
               style={ReactDOMRe.Style.make(~cursor="pointer", ())}
             />
           })
         ->React.array
       : React.null}
  </svg>;
};