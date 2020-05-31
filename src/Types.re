// state? Or something

// type pos = (float, float);

/*
 Ok let's say we have a circle
 and you put a point on the circle
 So the point is a child of the circle, with pos being like 0,0? OR something
 because the point is basically projected onto the circle.

 Ok so you first select the "base", and then the point is defined
 as a projection.
 So you could select the circle, and then have the point be not on the circle, but it would still react when the circle is resized.
 You could say 'is x pixels larger than the radius', or 'is x times the radius'




 So shapes have ...
 ... hm maybe the symmetries are just offshoots of this inheritence

 So
 hierarchy

 Circle(x, y, r)
     point(angle, offsetPixels | offsetPercent)

 hrrmmmm but then you have a line, where each end can have a different parent
 so what you have is maybe just points
 and then other shapes reference those points, like by ID? or path?
 "go to the third radial copy of the point...."



 Symmetries need to allow for missing ones.
 So
 6-fold, items: 1,2,3,4,5,6
 or
 6-fols, items: 1,2,4,5,6 (missing 3 because that's been specialized)

 Ok, so I'm pretty sure each concrete point needs to have a path.


 A point on a line can be a percent of the way between the two points









 */

type percentOrAbs =
  | Percent(float);
//   | Abs(float);

// Ok but what if instead of "string" here it could be
// "reference" which is either "actual" point (id)
// or "virtual", which is like "the nth symmetry around x point"?
// Orr wait I keep track of symmetries too ...
// Where a symmetry references a center point and has a count.
type reference = {
  id: string,
  index: int,
};

type pointPos =
  | Abs({
      x: float,
      y: float,
    })
  | Line({
      source: reference,
      dest: reference,
      percentOrAbs,
    })
  | Rotate({
      source: reference,
      dest: reference,
      theta: float,
    })
  | RotateBetween({
      one: reference,
      middle: reference,
      two: reference,
      amount: float,
    })
  | Circle({
      center: reference,
      onEdge: reference, // this is the second point needed to define the circle
      angle: float,
      offset: percentOrAbs,
    });
//   | ThreeCircle({
//       p1: reference,
//       p2: reference,
//       p3: reference,
//       // TODO This doesn't capture: "midpoint between p1 and p2"
//       anchor: [ | `p1 | `p2 | `p3],
//       angle: float,
//       offset: percentOrAbs,
//     })
// Ok I'm a radial symmetry of "point" about "center", number "index" of "count"
// | Radial({
//     center: reference,
//     point: reference,
//     count: int,
//     index: int,
//   });

type symm = {
  center: reference,
  // point: string,
  count: int,
};

// type shapeSymm = {
//   center: reference,
//   // shape: string,
//   count: int,
//   // missing: list(int) // should be a set
// };

// Do we have point symmetries, and, separately, shape symmetries?
// Yes I believe so, because shape symmetries can be "broken", right?
// Whereas it doesn't make sense for a point symmetry to be broken?
// hmmmm yeah I think that's the case?
// OK START HERE FOLKS
// for now, let's not break any symmetries.
// but yeah I do think it makes sense to have different symmetries for points vs shapes.

/*
 Ok, feeling good about these points in relation to each other.
 Now, I assume any shapes I define will be just "drawing"
 based on the points. Yeah.
 And in turn, placing points will use existing shapes as
 a guide for where to place them, in relation to what.

 For now I will reify all symmetrical things immediately.
 And see if I can abstract that out later on.

 */

/*
 Ok, so a point is defined by a path ...
 ok but we also need symmetries?

 Like:

 // but the symmetry? We've got several...
 // and if we want to change the symmetry ...
 // maybe that's a fool's game.
 // Like, there's not really a way to recover that.
 // Ok so you kinda have to commit to a certain symmetry.
 // umm wait but then we can't edit things in tandem.
 // Ok so we can't immediately reify all symmetries.

 // So... there are the ~few "real" points, and then a variety of virtual points.
 // Virtual points are "this point, but symmetried this way".
 // Ok so maybe that works fine.
 Circle(
     center: abs(0,0),
     onEdge: abs(0, 5),
     angle: 1.0,
     offset: 0.0,
 )

 */

// ok

type pos = {
  x: float,
  y: float,
};

type ccirclePart = {
  to_: pos,
  r: float,
  sweep: bool,
};

type concreteShape =
  | CLine({
      p1: pos,
      p2: pos,
    })
  | CCircle({
      center: pos,
      r: float,
    })
  | CCirclePart({
      center: pos,
      r: float,
      theta0: float,
      theta1: float,
    });
// | CPoly({
//     p0: pos,
//     items: list([ | `Line(pos) | `Arc(ccirclePart)]),
//   });

type lineOpts = {
  p1: reference,
  p2: reference,
};
type circlePartOpts = {
  // always clockwise, folks.
  center: reference,
  onEdge: reference,
  goUntil: reference,
};

type shapeKind =
  //   | Point(string)
  | Line({
      p1: reference,
      p2: reference,
    })
  | Circle({
      center: reference,
      onEdge: reference,
    })
  | CirclePart({
      // always clockwise, folks.
      center: reference,
      onEdge: reference,
      goUntil: reference,
    });
// | Poly(list([ | `Line(lineOpts) | `CirclePart(circlePartOpts)]));

//   | Arc({
//       p1: string,
//       p2: string,
//       p3: string,
//     });
// Maybe polygon? idk. that could be how I do fills.
// I mean not strictly polygon, because it would include arcs

// Selections can be either:
// a point, or a shape
// type kindSelection =
//   | LineSel([ | `One | `Two])
//   | CircleSel([ | `Pos | `Radius])
//   | ArcSel([ | `One | `Two | `Three]);

// type symmetrySelection =
//   | RadialSel(int)
//   | TranslSel({
//       x: int,
//       y: int,
//     });

// type symmetry =
//   | Radial({
//       pos,
//       radius: float,
//       angle: float,
//       times: int,
//     })
//   | Translational({
//       dx: float,
//       dy: float,
//       tx: int,
//       ty: int,
//     });

// type shape = {
//   id: string,
//   kind: shapeKind,
//   //   symmetry,
// };

type selection =
  | Points(list(reference))
  | Shapes(list(reference))
  | Tiles(list(reference));

type hover = [ | `Point(reference) | `Shape(reference)];
// type selection = {
//   kind: kindSelection,
//   sym: symmetrySelection,
// };

type tile = {
  color: string,
  margin: float,
  order: float,
  sides: list(reference),
  sym: option(symm),
};

type point = {
  pos: pointPos,
  sym: option(symm),
};
type shape = {
  kind: shapeKind,
  sym: option(symm),
  color: option(string),
};

type points = Belt.Map.String.t(point);
// type symmetries = Belt.Map.String.t(symm);
type positions = Hashtbl.t(string, pos);
type shapes = Belt.Map.String.t(shape);
type tiles = Belt.Map.String.t(tile);
// type shapeSymmetries = Belt.Map.String.t(shapeSymm);

type presentation = {
  points: bool,
  traces: bool,
  zoom: float,
  center: pos,
};

let defaultPresentation = {
  points: true,
  traces: true,
  zoom: 1.0,
  center: {
    x: 0.,
    y: 0.,
  },
};

type scene = {
  points,
  shapes,
  tiles,
  presentation,
};