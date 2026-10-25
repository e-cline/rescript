import { make as adminButton } from "./Admin/Button.gen";
import { panel } from "./Admin/Panel.gen";
import { both } from "./App.gen";
import { make as shadow } from "./TestsGentypeQualifiedSources/Shadow.gen";
import { make as uiButton } from "./Ui/Button.gen";

export const label: string = both(uiButton("a"), adminButton());
export const panelLabel: string = panel(uiButton("b"), adminButton());
export const shadowName: string = shadow("c").name;

// Ui.Button.t and Admin.Button.t are different types.
// @ts-expect-error
both(adminButton(), adminButton());
