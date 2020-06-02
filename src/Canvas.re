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

let tx = (x, transform) => (x -. transform.center.x) *. transform.zoom;
let ty = (y, transform) => (y -. transform.center.x) *. transform.zoom;
let tf = (f, transform) => f *. transform.zoom;

let s = Js.Float.toString;

let normalizeTheta = t => t < 0. ? Js.Math._PI *. 2. +. t : t;

let almostEqual = (p1, p2) =>
  abs_float(p1.x -. p2.x) < 0.001 && abs_float(p1.y -. p2.y) < 0.001;

let force = x =>
  switch (x) {
  | None => failwith("unwrapped empty")
  | Some(x) => x
  };

let findMap = (arr, fn) => {
  let rec loop = i =>
    if (i >= Array.length(arr)) {
      None;
    } else {
      let item = arr[i];
      switch (fn(item)) {
      | None => loop(i + 1)
      | Some(v) => Some((v, i))
      };
    };
  loop(0);
};

let polyPath = (transform, items, margin) => {
  // Js.log2("Poly Path", Belt.List.toArray(items));

  // how to sort things?
  // pick one.
  // go through until you fine the one at its tail
  // flip that if needed
  // keep going
  let pool = Belt.List.toArray(items);
  let ordered = [||];

  let rec loop = endp =>
    if (Array.length(pool) == 0) {
      ();
    } else {
      // Js.log2("lookgin for", endp);
      let found =
        pool->findMap(shape => {
          let (astartp, aendp) = Calculate.endPoints(shape);
          if (almostEqual(astartp, endp)) {
            Some((shape, aendp));
          } else if (almostEqual(aendp, endp)) {
            Some((Calculate.flip(shape), astartp));
          } else {
            None;
          };
        });
      switch (found) {
      | None => ()
      | Some(((shape, endp), idx)) =>
        pool->Js.Array2.spliceInPlace(~pos=idx, ~remove=1, ~add=[||])->ignore;
        ordered->Js.Array2.push(shape)->ignore;
        loop(endp);
      };
    };

  let first = pool->Js.Array2.pop->force;
  let (_, endp) = Calculate.endPoints(first);
  ordered->Js.Array2.push(first)->ignore;
  loop(endp);

  let ordered = Calculate.joinAdjacentLineSegments(ordered);

  let ordered = margin == 0. ? ordered : Calculate.inset(ordered, margin);

  // Js.log2("Ordered", ordered);

  ordered
  ->Belt.List.fromArray
  ->Belt.List.mapWithIndex((i, shape) => {
      let (startp, endp) = Calculate.endPoints(shape);
      (
        i == 0
          ? Printf.sprintf(
              "M %0.2f %0.2f ",
              tx(startp.x, transform),
              ty(startp.y, transform),
            )
          : ""
      )
      ++ (
        switch (shape) {
        | CLine({p2}) =>
          Printf.sprintf(
            "L %0.2f %0.2f",
            tx(p2.x, transform),
            ty(p2.y, transform),
          )
        | CCirclePart({r, theta0, theta1, clockwise}) =>
          let sweep = normalizeTheta(theta1 -. theta0) > Js.Math._PI;
          let sweep = clockwise ? sweep : !sweep;
          Printf.sprintf(
            {|A %0.2f %0.2f
          0
          %d %d
          %0.2f %0.2f|},
            tf(r, transform),
            tf(r, transform),
            sweep ? 1 : 0,
            clockwise ? 1 : 0,
            tx(endp.x, transform),
            ty(endp.y, transform),
          );
        | CCircle(_) => ""
        }
      );
    })
  |> String.concat(" ");
};

module Shape = {
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
      | CCirclePart({center, r, theta0, theta1, clockwise}) =>
        let start = {
          x: center.x +. cos(theta0) *. r,
          y: center.y +. sin(theta0) *. r,
        };
        let endd = {
          x: center.x +. cos(theta1) *. r,
          y: center.y +. sin(theta1) *. r,
        };
        let sweep = normalizeTheta(theta1 -. theta0) > Js.Math._PI;
        let sweep = clockwise ? sweep : !sweep;
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
            sweep ? 1 : 0,
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
              ? style([
                  hover([
                    unsafe("stroke-width", "7"),
                    unsafe("stroke", "rgba(100, 220, 255, 1.0)"),
                  ]),
                ])
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

let isTileSelected = (selection, r) =>
  switch (selection) {
  | Some(Tiles(tiles)) => tiles->Belt.List.has(r, (==))
  | _ => false
  };

let isTileSelectedOrHovered = (selection, r) =>
  switch (selection) {
  | Some(Tiles(tiles)) => tiles->Belt.List.some(s => s.id == r.id)
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

let cmp = (a, b) => a < b ? (-1) : a > b ? 1 : 0;

module Point = {
  [@react.component]
  let make =
      (~pos as {x, y}, ~onClick, ~transform, ~isSelected, ~isHovered, ~size) => {
    <circle
      cx={Js.Float.toString(tx(x, transform))}
      cy={Js.Float.toString(ty(y, transform))}
      onClick
      r={string_of_int(size)}
      fill="rgba(255,255,255,0.2)"
      stroke="rgba(0,0,0,0.5)"
      strokeWidth={isSelected || isHovered ? "3" : "1"}
      className=Css.(style([hover([unsafe("stroke-width", "5")])]))
      //  strokeWidth={isSelected(selection, k) ? "3" : "0"}
      style={ReactDOMRe.Style.make(~cursor="pointer", ())}
    />;
  };
};

[@react.component]
let make =
    (
      ~width,
      ~height,
      ~innerRef,
      ~hover: option(hover),
      ~scene: scene,
      ~selection: option(selection),
      ~selectPoint: reference => unit,
      ~selectShape: reference => unit,
      ~selectTile: reference => unit,
      ~setScene,
      ~setSelection,
    ) => {
  let (positions, points, shapes, tiles) =
    React.useMemo1(
      () => {Calculate.calculateAllPositions(scene)},
      [|scene|],
    );

  let potentials =
    React.useMemo2(
      () => Calculate.potentials(scene, selection, positions),
      (scene, selection),
    );

  let transform = {
    zoom: scene.presentation.transform.zoom,
    center:
      Calculate.addPos(
        scene.presentation.transform.center,
        {
          x: -. float_of_int(width) /. 2. /. scene.presentation.transform.zoom,
          y:
            -. float_of_int(height) /. 2. /. scene.presentation.transform.zoom,
        },
      ),
  };

  <svg
    xmlns="http://www.w3.org/2000/svg"
    ref={ReactDOMRe.Ref.domRef(innerRef)}
    width={string_of_int(width) ++ "px"}
    height={string_of_int(height) ++ "px"}>
    // {tiles->Js.Array2.sortInPlaceWith}

      {tiles
       ->Js.Array2.sortInPlaceWith(((_, _, a), (_, _, b)) =>
           cmp(a.order, b.order)
         )
       ->Belt.Array.map(((k, sides, {color, margin})) => {
           <path
             key={toId(k)}
             fill=color
             d={polyPath(transform, sides, margin)}
             onClick={_ => selectTile(k)}
             stroke={isTileSelected(selection, k) ? "black" : "green"}
             strokeWidth={isTileSelectedOrHovered(selection, k) ? "3" : "0"}
             className=Css.(
               style([
                 cursor(`pointer),
                 //  hover([
                 //  ])
               ])
             )
           />
         })
       ->React.array}
      {(
         scene.presentation.traces
           ? shapes : shapes->Belt.Array.keep(((_, _, c)) => c != None)
       )
       //  ->Js.Array2.sortInPlaceWith(((_, _, a), (_, _, b)) =>
       //      switch (a, b) {
       //      | (None, Some(_)) => (-1)
       //      | (Some(_), None) => 1
       //      | _ => 0
       //      }
       //    )
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
      {scene.presentation.points
         ? points
           ->Belt.Array.map(((k, pos)) => {
               <Point
                 key={toId(k)}
                 size=2
                 onClick={_ => selectPoint(k)}
                 transform
                 pos
                 isSelected={isSelected(selection, k)}
                 isHovered={hover == Some(`Point(k))}
               />
             })
           ->React.array
         : React.null}
      {potentials
       ->Belt.List.sort((a, b) =>
           switch (a, b) {
           | (`Point(_), `Shape(_)) => 1
           | (`Shape(_), `Point(_)) => (-1)
           | _ => 0
           }
         )
       ->Belt.List.toArray
       ->Belt.Array.mapWithIndex((i, item) =>
           switch (item) {
           | `Point(point, pos) =>
             <Point
               key={string_of_int(i)}
               onClick={_ => {
                 let (scene, k) =
                   scene->Api.Point.add(~sym=point.sym, point.pos);
                 setScene(scene);
                 setSelection(Some(Points([{id: k, index: 0}])));
                 // TODO
                 ();
               }}
               size=4
               transform
               pos
               isSelected=true
               isHovered=true
             />
           | `Shape(shape, concrete) =>
             <Shape
               color=None
               isHovered=true
               transform
               isSelected=true
               onSelect={() => {
                 let (scene, k) = scene->Api.Shape.addFull(shape);
                 setScene(scene);
                 setSelection(Some(Shapes([{id: k, index: 0}])));
               }}
               key={string_of_int(i)}
               shape=concrete
             />
           }
         )
       ->React.array}
    </svg>;
};