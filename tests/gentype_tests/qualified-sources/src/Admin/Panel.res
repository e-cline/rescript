// Button is the sibling Admin.Button.
@genType
let panel = (button: Ui.Button.t, own: Button.t) => button.label ++ Int.toString(own.count)
