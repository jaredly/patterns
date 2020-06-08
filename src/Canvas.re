open Types;

module Colors = {
  let selected = "rgba(0, 255, 0, 1.0)";
  let hovered = "#555";
  let creating = "magenta";
};

let tx = (x, transform) => (x -. transform.center.x) *. transform.zoom;
let ty = (y, transform) => (y -. transform.center.y) *. transform.zoom;
let tf = (f, transform) => f *. transform.zoom;
let tp = (p, transform) => {x: tx(p.x, transform), y: ty(p.y, transform)};

let s = Js.Float.toString;

let normalizeTheta = t => t < 0. ? Js.Math._PI *. 2. +. t : t;

let force = x =>
  switch (x) {
  | None => failwith("unwrapped empty")
  | Some(x) => x
  };

let arrow = (~r=8., pos, angle) => {
  let p0 = Calculate.push(pos, ~theta=angle, ~mag=r);
  let p1 =
    Calculate.push(
      pos,
      ~theta=angle +. Calculate.pi /. 3. *. 2.,
      ~mag=r /. 2.,
    );
  let p2 =
    Calculate.push(
      pos,
      ~theta=angle -. Calculate.pi /. 3. *. 2.,
      ~mag=r /. 2.,
    );
  <path
    fill="blue"
    d={Printf.sprintf(
      "M %0.2f %0.2f L %0.2f %0.2f L %0.2f %0.2f Z",
      p0.x,
      p0.y,
      p1.x,
      p1.y,
      p2.x,
      p2.y,
    )}
  />;
};

let square = (~r=4., {x, y}) =>
  <rect
    x={Js.Float.toString(x -. r)}
    y={Js.Float.toString(y -. r)}
    width={Js.Float.toString(r *. 2.)}
    height={Js.Float.toString(r *. 2.)}
  />;

let dot = (~r=4., {x, y}) =>
  <circle
    cx={Js.Float.toString(x)}
    cy={Js.Float.toString(y)}
    r={Js.Float.toString(r)}
  />;

let shapeDebugPoints = (transform, shape) =>
  switch (shape) {
  | CLine({p1, p2}) => [
      square(tp(p1, transform)),
      dot(tp(p2, transform)),
    ]
  | CCircle({center, r}) => [square(tp(center, transform))]
  | CCirclePart({center, r, theta0, theta1, clockwise}) =>
    let p1 = Calculate.push(center, ~theta=theta0, ~mag=r);
    let p2 = Calculate.push(center, ~theta=theta1, ~mag=r);
    let span = PolyLine.angleBetween(theta0, theta1, clockwise);
    let thetaMid = theta0 +. span /. 2.;
    [
      square(tp(p1, transform)),
      dot(tp(p2, transform)),
      arrow(
        tp(Calculate.push(center, ~theta=thetaMid, ~mag=r), transform),
        thetaMid +. (clockwise ? Calculate.pi2 : -. Calculate.pi2),
      ),
    ];
  };

let pathToShapeEnd = (transform, shape) => {
  switch (shape) {
  | CLine({p2}) =>
    Printf.sprintf(
      "L %0.2f %0.2f",
      tx(p2.x, transform),
      ty(p2.y, transform),
    )
  | CCirclePart({center, r, theta0, theta1, clockwise}) =>
    let sweep = normalizeTheta(theta1 -. theta0) > Js.Math._PI;
    let sweep = clockwise ? sweep : !sweep;
    let endp = Calculate.push(center, ~theta=theta1, ~mag=r);
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
  };
};

let pathForShape = (transform, shape) => {
  let (startp, _) = Calculate.endPoints(shape);
  Printf.sprintf(
    "M %0.2f %0.2f ",
    tx(startp.x, transform),
    ty(startp.y, transform),
  )
  ++ pathToShapeEnd(transform, shape);
};

let polyPath = (transform, items, margin, debug) => {
  let ordered = PolyLine.orderItems(items);
  let ordered = PolyLine.joinAdjacentLineSegments(ordered);
  let ordered =
    margin == 0. ? ordered : PolyLine.inset(ordered, margin, debug);

  // Js.log2("Ordered", ordered);

  let res =
    ordered
    ->Belt.List.fromArray
    ->Belt.List.mapWithIndex((i, shape) => {
        let (startp, _) = Calculate.endPoints(shape);
        (
          i == 0
            ? Printf.sprintf(
                "M %0.2f %0.2f ",
                tx(startp.x, transform),
                ty(startp.y, transform),
              )
            : ""
        )
        ++ pathToShapeEnd(transform, shape);
      })
    |> String.concat(" ");
  res ++ "z";
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
                    unsafe("stroke-width", "8"),
                    unsafe("stroke", "rgb(0, 200, 0)"),
                  ]),
                ])
              : style([
                  hover([
                    unsafe(
                      "stroke",
                      isHovered ? "rgb(50, 180, 230" : "rgb(100, 220, 255)",
                    ),
                  ]),
                ])
          )
          onClick={_ => onSelect()}
          style={ReactDOMRe.Style.make(~cursor="pointer", ())}
          strokeWidth="6"
          stroke={
            isSelected
              ? "rgb(0, 255, 0)"
              : isHovered ? "rgba(100, 220, 255)" : "rgba(0,0,0,0)"
          }
        />
        <Inner
          shape
          transform
          onClick={_ => ()}
          className=Css.(
            style(
              [
                pointerEvents(`none),
                opacity(isHovered || isSelected ? 1. : 0.5),
              ]
              @ (isHovered ? [unsafe("stroke-dasharray", "3")] : []),
            )
          )
          style={ReactDOMRe.Style.make(~cursor="pointer", ())}
          strokeWidth={isSelected ? "4" : "2"}
          stroke={
            switch (color) {
            | None => "rgb(255, 0, 255)"
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

let isShapeSelected = (selection: selection, r) =>
  selection.shapes->Belt.List.has(r, (==));

let isTileSelected = (selection: selection, r) =>
  selection.tiles->Belt.List.has(r, (==));

let isTileSelectedOrHovered = (selection: selection, r) =>
  selection.tiles->Belt.List.some(s => s.id == r.id);

let isShapeHovered = (selection: selection, r) =>
  selection.shapes->Belt.List.some(s => s.id == r.id);

let isSelected = (selection: selection, r) =>
  selection.points->Belt.List.has(r, (==));

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
      stroke={isSelected ? "rgba(0,255,0,0.8)" : "rgba(0,0,0,0.5)"}
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
      ~selection: selection,
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
      () => Potentials.potentials(scene, selection, positions),
      (scene, selection),
    );

  let potentialTiles =
    potentials->Belt.List.keepMap(i =>
      switch (i) {
      | `Tile(t) => Some(t)
      | _ => None
      }
    );

  let potentialShapes =
    potentials->Belt.List.keepMap(i =>
      switch (i) {
      | `Shape(t) => Some(t)
      | _ => None
      }
    );

  let potentialPoints =
    potentials->Belt.List.keepMap(i =>
      switch (i) {
      | `Point(t) => Some(t)
      | _ => None
      }
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
             d={polyPath(transform, sides, margin, false)}
             onClick={_ => {
               selectTile(k);
               Js.log("OK SELECTED TILE");
               Js.log(
                 "["
                 ++ (
                   sides->Belt.List.map(showConcreteShape)
                   |> String.concat(",\n  ")
                 )
                 ++ "]",
               );
             }}
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
      {potentialTiles
       ->Belt.List.toArray
       ->Belt.Array.mapWithIndex(
           (i, (fullSides, {sym, color, margin, sides})) => {
           <path
             key={string_of_int(i)}
             fill=color
             d={polyPath(transform, fullSides, margin, false)}
             //  onClick={_ => selectTile(k)}
             onClick={_ => {
               let (scene, k) = scene->Api.Tile.add(~sym, sides);
               setScene(scene);
               setSelection({
                 tiles: [{id: k, index: 0}],
                 shapes: [],
                 points: [],
               });
             }}
             stroke="black"
             strokeWidth="3"
             className=Css.(style([cursor(`pointer)]))
           />
         })
       ->React.array}
      {(
         scene.presentation.traces
           ? shapes : shapes->Belt.Array.keep(((_, _, c)) => c != None)
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
      {potentialShapes
       ->Belt.List.toArray
       ->Belt.Array.mapWithIndex((i, (shape, concreteShape)) => {
           <Shape
             color=None
             isHovered=true
             transform
             isSelected=true
             onSelect={() => {
               let (scene, k) = scene->Api.Shape.addFull(shape);
               setScene(scene);
               setSelection({
                 shapes: [{id: k, index: 0}],
                 tiles: [],
                 points: [],
               });
             }}
             key={string_of_int(i)}
             shape=concreteShape
           />
         })
       ->React.array}
      {potentialPoints
       ->Belt.List.toArray
       ->Belt.Array.mapWithIndex((i, (point, pos)) =>
           <Point
             key={string_of_int(i)}
             onClick={_ => {
               let (scene, k) =
                 scene->Api.Point.add(~sym=point.sym, point.pos);
               setScene(scene);
               setSelection({
                 points: [{id: k, index: 0}],
                 shapes: [],
                 tiles: [],
               });
               // TODO
               ();
             }}
             size=4
             transform
             pos
             isSelected=true
             isHovered=true
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
    </svg>;
};