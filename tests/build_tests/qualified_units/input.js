// @ts-check

// Compiles a qualified source tree the way a build system does it: one
// namespace map per directory, and one compilation unit per source file,
// named after its directory path, such as `Button-Controls-Ui-MyApp` for
// src/Ui/Controls/Button.res.

import * as assert from "node:assert/strict";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { pathToFileURL } from "node:url";
import { setup } from "#dev/process";

const root = import.meta.dirname;
const { bsc } = setup(root);
const libDir = path.join(root, "lib");
const buildDir = path.join(libDir, "bs");
const directories = [
  "src",
  "src/Ui",
  "src/Ui/Controls",
  "src/Domain",
  "src/Admin",
];
const includes = ["-I", buildDir].concat(
  directories.flatMap(directory => ["-I", path.join(buildDir, directory)]),
);

/**
 * @param {string} name
 * @param {string[]} members
 */
async function namespaceMap(name, members) {
  await fs.writeFile(
    path.join(buildDir, `${name}.mlmap`),
    ["randjbuildsystem", ...members, ""].join("\n"),
  );
  const result = await bsc(["-w", "-49", "-no-alias-deps", `${name}.mlmap`], {
    cwd: buildDir,
  });
  assert.equal(result.status, 0, `${name}: ${result.stdout}${result.stderr}`);
}

/**
 * @param {string} directory
 * @param {string} module
 * @param {string[]} namespaceArgs
 */
async function compile(directory, module, namespaceArgs) {
  const ast = `${directory}/${module}.ast`;
  await fs.mkdir(path.join(buildDir, directory), { recursive: true });
  await fs.mkdir(path.join(libDir, "js", directory), { recursive: true });
  const parsed = await bsc(
    ["-bs-ast", "-o", ast, path.join(root, directory, `${module}.res`)],
    { cwd: buildDir },
  );
  assert.equal(parsed.status, 0, `${ast}: ${parsed.stdout}${parsed.stderr}`);
  const result = await bsc(
    [
      ...namespaceArgs,
      ...includes,
      "-bs-project-root",
      root,
      "-bs-package-name",
      "qualified_units",
      "-bs-package-output",
      `esmodule:lib/js/${directory}:.mjs`,
      ast,
    ],
    { cwd: buildDir },
  );
  assert.equal(result.status, 0, `${ast}: ${result.stdout}${result.stderr}`);
}

/** @param {string} ast */
async function astDependencies(ast) {
  const contents = await fs.readFile(path.join(buildDir, ast));
  const length = contents.readInt32BE(0);
  return contents
    .subarray(4, 4 + length)
    .toString("utf8")
    .split("\n")
    .filter(name => name !== "");
}

await fs.rm(libDir, { recursive: true, force: true });
await fs.mkdir(buildDir, { recursive: true });

try {
  await namespaceMap("MyApp", ["Admin", "App", "Domain", "Ui"]);
  await namespaceMap("Ui-MyApp", ["Button", "Controls", "Input"]);
  await namespaceMap("Controls-Ui-MyApp", ["Button"]);
  await namespaceMap("Domain-MyApp", ["User"]);
  await namespaceMap("Admin-MyApp", ["Button"]);

  await compile("src/Domain", "User", [
    "-bs-open-ns",
    "MyApp",
    "-bs-ns",
    "Domain-MyApp",
  ]);
  await compile("src/Ui", "Button", [
    "-bs-open-ns",
    "MyApp",
    "-bs-ns",
    "Ui-MyApp",
  ]);
  await compile("src/Ui", "Input", [
    "-bs-open-ns",
    "MyApp",
    "-bs-ns",
    "Ui-MyApp",
  ]);
  await compile("src/Ui/Controls", "Button", [
    "-bs-open-ns",
    "MyApp",
    "-bs-open-ns",
    "Ui-MyApp",
    "-bs-ns",
    "Controls-Ui-MyApp",
  ]);
  await compile("src/Admin", "Button", [
    "-bs-open-ns",
    "MyApp",
    "-bs-ns",
    "Admin-MyApp",
  ]);
  await compile("src", "App", ["-bs-ns", "MyApp"]);

  // The header names each prefix of a qualified path, so a build system can
  // find the source module that a path such as Ui.Controls.Button names.
  assert.deepEqual(await astDependencies("src/App.ast"), [
    "Admin",
    "Admin.Button",
    "Ui",
    "Ui.Button",
    "Ui.Button.Icon",
    "Ui.Controls",
    "Ui.Controls.Button",
  ]);

  // JavaScript files are named after their source files, not their units.
  for (const file of [
    "src/App.mjs",
    "src/Ui/Button.mjs",
    "src/Ui/Input.mjs",
    "src/Ui/Controls/Button.mjs",
    "src/Admin/Button.mjs",
    "src/Domain/User.mjs",
  ]) {
    await fs.access(path.join(libDir, "js", file));
  }
  const app = await import(
    pathToFileURL(path.join(libDir, "js", "src", "App.mjs")).href
  );
  assert.equal(app.primary, "ui-button:a");
  assert.equal(app.compact, "compact:ui-button:b/user:b");
  assert.equal(app.admin, "admin:ui-button:admin");
  assert.equal(app.icon, "icon");

  // A new member of the package map changes MyApp.cmi. The qualified units
  // that open MyApp don't record its CRC, so App still compiles against
  // them without rebuilding them.
  await namespaceMap("MyApp", ["Admin", "App", "Domain", "Extra", "Ui"]);
  await compile("src", "Extra", ["-bs-ns", "MyApp"]);
  await compile("src", "App", ["-bs-ns", "MyApp"]);
} finally {
  await fs.rm(libDir, { recursive: true, force: true });
}
