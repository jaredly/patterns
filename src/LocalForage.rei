type key('a);
let toKey: string => key('a);
let setItem: (key('a), 'a) => Js.Promise.t(unit);
let getItem: key('a) => Js.Promise.t(option('a));
let keys: unit => Js.Promise.t(array(string));