open Types;

module S = Belt.Map.String;
let empty = S.empty;

let init = () => {
  points: empty,
  // symmetries: empty,
  shapes: empty,
  // shapeSymmetries: empty,
};

let genId = () =>
  Js.Math.random()
  ->Js.Float.toStringWithRadix(~radix=36)
  ->Js.String2.sliceToEnd(~from=2);

let percent = x => Percent(x);

module Ref = {
  let id = id => {id, index: 0};
  let sym = (id, index) => {id, index};
};

module Point = {
  let add = (scene, ~sym=None, pos) => {
    let point = {pos, sym};
    let id = genId();
    ({...scene, points: S.set(scene.points, id, point)}, id);
  };
  let abs = (scene, ~sym=None, x, y) => add(scene, ~sym, Abs({x, y}));
  let line = (scene, ~sym=None, source, dest, percent) =>
    add(scene, ~sym, Line({source, dest, percentOrAbs: Percent(percent)}));
  let circle = (scene, ~sym=None, center, onEdge, angle, offset) =>
    add(
      scene,
      ~sym,
      Circle({center, onEdge, angle, offset: Percent(offset)}),
    );
  // let sym = (scene, point, center, count) => {
  //   let id = genId();
  //   (
  //     {
  //       ...scene,
  //       symmetries: S.set(scene.symmetries, id, {center, point, count}),
  //     },
  //     id,
  //   );
  // };
};

module Shape = {
  let add = (scene, ~sym=None, shape) => {
    let shape = {kind: shape, sym};
    let id = genId();
    ({...scene, shapes: S.set(scene.shapes, id, shape)}, id);
  };
  let line = (scene, ~sym=None, p1, p2) => add(scene, ~sym, Line({p1, p2}));
  let circle = (scene, ~sym=None, center, onEdge) =>
    add(scene, ~sym, Circle({center, onEdge}));
  // let sym = (scene, shape, center, count) => {
  //   let id = genId();
  //   (
  //     {
  //       ...scene,
  //       shapeSymmetries:
  //         S.set(scene.shapeSymmetries, id, {center, shape, count}),
  //     },
  //     id,
  //   );
  // };
};