// `Button` is the sibling Ui.Button, and `Domain` comes from the package root.
let render = s => Button.make(s) ++ "/" ++ Domain.User.make(s)
