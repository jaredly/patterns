// Hello
open Types;
open Calculate;

let circleTest = (c1, r1, c2, r2) => {
  let crosses = Calculate.intersectCircles(c1, r1, c2, r2);
  switch (crosses) {
  | [p1, p2] => [
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: true,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta0: angleTo(dpos(c2, p1)),
          theta1: angleTo(dpos(c2, p2)),
          clockwise: true,
        }),
      ],
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: false,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta0: angleTo(dpos(c2, p1)),
          theta1: angleTo(dpos(c2, p2)),
          clockwise: true,
        }),
      ],
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: false,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta0: angleTo(dpos(c2, p1)),
          theta1: angleTo(dpos(c2, p2)),
          clockwise: false,
        }),
      ],
      [
        CCirclePart({
          center: c1,
          r: r1,
          theta0: angleTo(dpos(c1, p1)),
          theta1: angleTo(dpos(c1, p2)),
          clockwise: true,
        }),
        CCirclePart({
          center: c2,
          r: r2,
          theta0: angleTo(dpos(c2, p1)),
          theta1: angleTo(dpos(c2, p2)),
          clockwise: false,
        }),
      ],
    ]
  | _ => assert(false)
  };
};

[@react.component]
let make = () => {
  let tests =
    [|
      circleTest({x: 40., y: 40.}, 50., {x: 100., y: 100.}, 75.),
      circleTest({x: 70., y: 70.}, 50., {x: 100., y: 100.}, 75.),
    |]
    ->Belt.List.concatMany
    ->Array.of_list;
  <div>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={string_of_int(1000) ++ "px"}
      height={string_of_int(1000) ++ "px"}>
      {tests
       ->Belt.Array.mapWithIndex((i, sides) =>
           <path
             key={string_of_int(i)}
             fill="red"
             d={Canvas.polyPath(
               {
                 center: {
                   x: (-50.) -. float_of_int(i mod 4) *. 200.,
                   y: (-50.) -. float_of_int(i / 4) *. 200.,
                 },
                 zoom: 1.,
               },
               sides,
               0.,
             )}
             stroke="black"
             strokeWidth="3"
             className=Css.(
               style([
                 cursor(`pointer),
                 opacity(0.5),
                 //  hover([
                 //  ])
               ])
             )
           />
         )
       ->React.array}
    </svg>
  </div>;
};