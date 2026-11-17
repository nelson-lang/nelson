---
name: nelson-usage-app-ui
audience: user
modules: graphics, gui
description: Build simple graphical user interfaces and dialogs in Nelson - windows, controls, menus, file pickers and message boxes. Use when the user wants an interactive window, buttons, or a dialog.
triggers: interface, gui, ui, fenetre, window, bouton, button, controle, uicontrol, menu, uimenu, panneau, panel, dialogue, dialog, boite de dialogue, message, msgbox, question, questdlg, saisie, inputdlg, choisir fichier, uigetfile, callback, widget
functions: figure, uicontrol, uimenu, uipanel, uifigure, uicontextmenu, msgbox, questdlg, inputdlg, warndlg, errordlg, listdlg, uigetfile, uiputfile, waitbar, dialog
---

# Building a GUI and Dialogs

Create a window with controls and callbacks, or ask the user with a dialog.

## When to use
- A small interactive window: buttons, sliders, text, menus, panels.
- A dialog: message, question, text input, file picker, progress bar.

## When NOT to use
- Plotting data (use the plotting guide).

## Key functions
Windows/containers: `figure`, `uifigure`, `uipanel`, `uicontextmenu`. Controls/menus: `uicontrol`, `uimenu`. Dialogs: `msgbox`, `questdlg`, `inputdlg`, `warndlg`, `errordlg`, `listdlg`, `uigetfile`, `uiputfile`, `waitbar`, `dialog`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Create the window (`figure`/`uifigure`), then add controls with `uicontrol(parent, 'Style', ..., 'Position', [x y w h], ...)`.
2. Wire behaviour with a `Callback` set to a function handle; keep the callback small and name it.
3. Group related controls in a `uipanel`; position in pixels relative to the parent.
4. For a quick interaction, prefer a dialog (`questdlg`, `inputdlg`, `uigetfile`) over a full window.
5. Show long-running progress with `waitbar`, and close it when done.

## Gotchas
- `Position` is `[left bottom width height]`; the origin is bottom-left.
- A callback runs later - capture the data it needs (handles, values) rather than relying on loop variables.
- A dialog returns the user's choice (or empty on cancel); always handle the cancel case.

## Done when
The window/dialog shows the requested controls, callbacks respond correctly, and cancel/close paths are handled.
