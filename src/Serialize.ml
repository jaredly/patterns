type preJson
let _toJson: preJson -> Js.Json.t = fun%raw data -> {|
  if (!data) {
    return data
  } else if (typeof data === 'function') {
    throw new Error("Cannot serialize a function")
  } else if (Array.isArray(data)) {
    if (data.tag != null) {
      return {
        $$tag: data.tag,
        $$contents: data.map(_toJson),
        $$bsVariant: data[Symbol.for('BsVariant')],
      }
    } else if (data[Symbol.for('BsVariant')] != null) {
      return {
        $$bsVariant: data[Symbol.for('BsVariant')],
        $$contents: data.map(_toJson)
      }
    } else if (data[Symbol.for('BsLocalModule')] != null) {
      return {
        $$bsLocalModule: data[Symbol.for('BsLocalModule')],
        $$contents: data.map(_toJson)
      }
    } else if (data[Symbol.for('BsPolyVar')] != null) {
      return {
        $$bsPolyVar: data[Symbol.for('BsPolyVar')],
        $$contents: data.map(_toJson)
      }
    } else if (data[Symbol.for('BsRecord')] != null) {
      return {
        $$bsRecord: data[Symbol.for('BsRecord')],
        $$contents: data.map(_toJson)
      }
    } else {
      return data.map(_toJson)
    }
  } else if (typeof data == 'object') {
    var result = {}
    Object.keys(data).forEach(key => result[key] = _toJson(data[key]))
    return result
  } else {
    return data
  }
|}

(** This dance is required to appease the type checker -- doing 
 * `toJson: 'a -> t = fun%raw` gives the error "contains type variables
 * that cannot be generalized". *)
external toT : 'a -> preJson = "%identity"
let serializeAnyToJson data = _toJson (toT data)

let unserializeAnyFromJsonUnsafe: Js.Json.t -> 'a = fun%raw data -> {|
  if (!data) {
    return data
  } else if (typeof data == 'object') {
    if (Array.isArray(data)) {
      return data.map(unserializeAnyFromJsonUnsafe)
    } else if (data.$$contents) {
      var result = data.$$contents.map(unserializeAnyFromJsonUnsafe)
      if (data.$$tag != null) {
        result.tag = data.$$tag
      }
      if (data.$$bsRecord) {
        result[Symbol.for('BsRecord')] = data.$$bsRecord
      }
      if (data.$$bsPolyVar) {
        result[Symbol.for('BsPolyVar')] = data.$$bsPolyVar
      }
      if (data.$$bsVariant) {
        result[Symbol.for('BsVariant')] = data.$$bsVariant
      }
      if (data.$$bsLocalModule) {
        result[Symbol.for('BsLocalModule')] = data.$$bsLocalModule
      }
      return result
    } else {
      var result = {}
      Object.keys(data).forEach(key => result[key] = unserializeAnyFromJsonUnsafe(data[key]))
      return result
    }
  } else {
    return data
  }
|} 