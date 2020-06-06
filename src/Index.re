// Entry point

[@bs.val] external document: Js.t({..}) = "document";

let hash = Controls.Location.hash();

if (hash == "#test") {
  ReactDOMRe.render(<Tests />, document##getElementById("root"));
} else {
  App.loadInitial()
  |> Js.Promise.then_(initial => {
       ReactDOMRe.render(<App initial />, document##getElementById("root"));
       Js.Promise.resolve();
     })
  |> ignore;
};