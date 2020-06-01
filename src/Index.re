// Entry point

[@bs.val] external document: Js.t({..}) = "document";

App.loadInitial()
|> Js.Promise.then_(initial => {
     ReactDOMRe.render(<App initial />, document##getElementById("root"));
     Js.Promise.resolve();
   });