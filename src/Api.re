open Types;

module S = Belt.Map.String;
let empty = S.empty;

let init = () => {
  points: empty,
  // symmetries: empty,
  shapes: empty,
  tiles: empty,
  presentation: defaultPresentation,
  // shapeSymmetries: empty,
};

[@bs.send] external padStart: (string, int, string) => string = "padStart";

let genId = () =>
  Js.Date.now()->Js.Float.toStringWithRadix(~radix=36)->padStart(10, "0")
  ++ "-"
  ++ Js.Math.random()
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
    Js.log(id);
    ({...scene, points: S.set(scene.points, id, point)}, id);
  };
  let abs = (scene, ~sym=None, x, y) => add(scene, ~sym, Abs({x, y}));
  let line = (scene, ~sym=None, source, dest, percent) =>
    add(scene, ~sym, Line({source, dest, percentOrAbs: Percent(percent)}));
  let rotateBetween = (scene, ~sym=None, one, middle, two, amount) =>
    add(scene, ~sym, RotateBetween({one, middle, two, amount}));
  let rotate = (scene, ~sym=None, source, dest, theta) =>
    add(scene, ~sym, Rotate({source, dest, theta}));
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

module Tile = {
  let add = (scene, ~sym, sides) => {
    let id = genId();
    let tile = {sides, sym, color: "blue", margin: 2., order: 0.};
    ({...scene, tiles: scene.tiles->S.set(id, tile)}, id);
  };
};

module Shape = {
  let setColor = (scene, id, color) => {
    Js.log2(id, S.keysToArray(scene.shapes));
    let shape = scene.shapes->S.getExn(id);
    {...scene, shapes: scene.shapes->S.set(id, {...shape, color})};
  };
  let add = (scene, ~sym=None, shape) => {
    let shape = {kind: shape, sym, color: None};
    let id = genId();
    ({...scene, shapes: S.set(scene.shapes, id, shape)}, id);
  };
  let line = (scene, ~sym=None, p1, p2) => add(scene, ~sym, Line({p1, p2}));
  let circle = (scene, ~sym=None, center, onEdge) =>
    add(scene, ~sym, Circle({center, onEdge}));
  let circlePart = (scene, ~sym=None, center, onEdge, goUntil) =>
    add(scene, ~sym, CirclePart({center, onEdge, goUntil}));
  // let poly = (scene, ~sym=None, items) => add(scene, ~sym, Poly(items));
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