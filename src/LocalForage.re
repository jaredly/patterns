[@bs.module "localforage"] external config: Js.t('a) => unit = "config";

config({"name": "patterns", "storeName": "patterns"});

[@bs.module "localforage"]
external _setItem: (string, 'a) => Js.Promise.t(unit) = "setItem";
[@bs.module "localforage"]
external _getItem: string => Js.Promise.t(option('a)) = "getItem";

[@bs.module "localforage"]
external keys: unit => Js.Promise.t(array(string)) = "keys";

type key('a) = string;
let toKey = (raw: string): key('a) => raw;

let setItem = (key: key('a), value: 'a) => _setItem(key, value);
let getItem = (key: key('a)): Js.Promise.t(option('a)) => _getItem(key);