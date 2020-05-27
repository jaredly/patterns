// Entry point

[@bs.val] external document: Js.t({..}) = "document";

module App = {
  [@react.component]
  let make = () => {
    <div> {React.string("hi")} </div>;
  };
};

let scene = {
  open Api;
  let scene = init();
  let (scene, id1) = scene->Point.abs(100., 100.);
  let (scene, id2) = scene->Point.abs(150., 150.);
  // let (scene, sm1) = scene->Point.sym(id1, Ref.id(id2), 5);
  // let (scene, id4) = scene->Point.line(Ref.id(id1), Ref.sym(sm1, 1), 0.5);
  // let (scene, sm1) = scene->Point.sym(id4, Ref.id(id2), 10);
  let (scene, id3) =
    scene->Point.circle(
      Ref.id(id1),
      Ref.id(id2),
      Js.Math._PI /. 3. *. 2.,
      1.0,
    );
  let (scene, id4) = scene->Point.line(Ref.id(id1), Ref.id(id3), 0.5);
  let (scene, id5) = scene->Point.line(Ref.id(id2), Ref.id(id1), 2.);

  let (scene, _sym) = scene->Point.sym(id3, Ref.id(id2), 6);
  let (scene, sym1) = scene->Point.sym(id1, Ref.id(id2), 6);

  let (scene, s1) = scene->Shape.line(Ref.id(id5), Ref.sym(sym1, 5));
  let (scene, _sym) = scene->Shape.sym(s1, Ref.id(id2), 6);

  let (scene, s2) = scene->Shape.circle(Ref.id(id1), Ref.id(id3));
  let (scene, _sym) = scene->Shape.sym(s2, Ref.id(id2), 6);

  let (scene, s2) = scene->Shape.circle(Ref.id(id4), Ref.id(id1));
  let (scene, _sym) = scene->Shape.sym(s2, Ref.id(id2), 6);

  scene;
};

ReactDOMRe.render(<Canvas scene />, document##getElementById("root"));