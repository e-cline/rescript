// A directory with the package namespace's name, which genType must not take
// for the namespace when it resolves the types of other modules.
@genType
type t = {name: string}

@genType
let make = name => {name: name}
