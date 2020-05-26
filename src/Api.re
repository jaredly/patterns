open Types;

module S = Belt.Map.String;
let empty = S.empty;

let init = () => {
  points: empty,
  symmetries: empty,
  shapes: empty,
  shapeSymmetries: empty,
};

let genId = () =>
  Js.Math.random()
  ->Js.Float.toStringWithRadix(~radix=36)
  ->Js.String2.sliceToEnd(~from=2);

let percent = x => Percent(x);

module Ref = {
  let id = id => Actual(id);
  let sym = (symmetry, index) => Virtual({symmetry, index});
};

module Point = {
  let add = (scene, point) => {
    let id = genId();
    ({...scene, points: S.set(scene.points, id, point)}, id);
  };
  let abs = (scene, x, y) => add(scene, Abs({x, y}));
  let line = (scene, source, dest, percent) =>
    add(scene, Line({source, dest, percentOrAbs: Percent(percent)}));
  let circle = (scene, center, onEdge, angle, offset) =>
    add(scene, Circle({center, onEdge, angle, offset: Percent(offset)}));

  let sym = (scene, point, center, count) => {
    let id = genId();
    (
      {
        ...scene,
        symmetries: S.set(scene.symmetries, id, {center, point, count}),
      },
      id,
    );
  };
};

module Shape = {
  let add = (scene, shape) => {
    let id = genId();
    ({...scene, shapes: S.set(scene.shapes, id, shape)}, id);
  };
  let line = (scene, p1, p2) => add(scene, Line({p1, p2}));
  let circle = (scene, center, onEdge) =>
    add(scene, Circle({center, onEdge}));

  let sym = (scene, shape, center, count) => {
    let id = genId();
    (
      {
        ...scene,
        shapeSymmetries:
          S.set(scene.shapeSymmetries, id, {center, shape, count}),
      },
      id,
    );
  };
};