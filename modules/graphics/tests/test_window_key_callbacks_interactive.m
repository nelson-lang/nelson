%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Name', 'Window key callback test', 'NumberTitle', 'off', ...
  'Position', [300 300 520 230], ...
  'WindowKeyPressFcn', @(source, event) ...
  windowKeyCallbackDisplay(source, event, true), ...
  'WindowKeyReleaseFcn', @(source, event) ...
  windowKeyCallbackDisplay(source, event, false));

uicontrol('Parent', f, 'Style', 'text', ...
  'String', 'Click in the edit field, press some keys, then press Esc to close.', ...
  'Position', [25 175 470 28], 'FontSize', 12, ...
  'HorizontalAlignment', 'center');
pressedText = uicontrol('Parent', f, 'Style', 'text', ...
  'String', 'Pressed: -', 'Position', [25 125 470 30], ...
  'FontName', 'Monospace', 'FontSize', 12, ...
  'HorizontalAlignment', 'left');
releasedText = uicontrol('Parent', f, 'Style', 'text', ...
  'String', 'Released: -', 'Position', [25 85 470 30], ...
  'FontName', 'Monospace', 'FontSize', 12, ...
  'HorizontalAlignment', 'left');
uicontrol('Parent', f, 'Style', 'edit', 'String', '', ...
  'Position', [120 30 280 32], 'FontSize', 12, ...
  'HorizontalAlignment', 'left');
set(f, 'UserData', struct('PressedText', pressedText, ...
  'ReleasedText', releasedText));

disp('Click the edit field and press keys. Press Esc to close.');
while isgraphics(f)
  drawnow('limitrate');
  pause(0.01);
end
%=============================================================================
function windowKeyCallbackDisplay(source, event, pressed)
  if ~isgraphics(source)
    return
  end
  keyName = event.Key;
  character = event.Character;
  if isempty(character)
    character = '<empty>';
  end
  modifiers = event.Modifier;
  if isempty(modifiers)
    modifierText = 'none';
  else
    modifierText = strjoin(modifiers, '+');
  end
  if pressed
    action = 'Pressed';
  else
    action = 'Released';
  end
  message = sprintf('%s: Key=%s  Character=%s  Modifiers=%s', ...
    action, keyName, character, modifierText);
  state = get(source, 'UserData');
  if pressed
    set(state.PressedText, 'String', message);
  else
    set(state.ReleasedText, 'String', message);
  end
  disp(message);
  if pressed && any(strcmpi(keyName, {'Esc', 'Escape'}))
    delete(source);
  end
end
%=============================================================================
