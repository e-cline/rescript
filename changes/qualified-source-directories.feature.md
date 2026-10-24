- Add qualified source directories: with `"subdirs": "qualified"` on a
  source, each directory below it is a module, so `src/Ui/Button.res` is
  `Ui.Button` and can sit beside `src/Admin/Button.res`. Directories are
  compile-time aliases, the JavaScript keeps each file's path, and a module
  sees the modules of its own and enclosing directories by their plain names.
  Editor analysis and genType follow the qualified paths. Only `rescript`
  supports it so far; `rescript-rust` rejects the setting.
