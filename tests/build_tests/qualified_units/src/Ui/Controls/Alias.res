// Paths through aliases of directories name the modules they reach.
module U = Ui
module C = Ui.Controls
let render = s => U.Input.render(s) ++ "/" ++ C.Button.make(s)
