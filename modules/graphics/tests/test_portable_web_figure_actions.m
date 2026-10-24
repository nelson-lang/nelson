%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off', 'Position', [20 30 400 300]);
ax = axes('Parent', f, 'XLim', [0 10], 'YLim', [0 20]);
plot(ax, [0 10], [0 20]);
drawnow();

assert_istrue(__web_figure_size__(f, 320, 240));
displayList = jsondecode(__web_display_list__(f));
assert_isequal([displayList.width displayList.height], [320 240]);

assert_istrue(__web_figure_pan__(f, 0.1, -0.2));
assert_isapprox(ax.XLim, [-1 9], 1e-12);
assert_isapprox(ax.YLim, [-4 16], 1e-12);

f.UserData = [];
f.WindowKeyPressFcn = @(source, event) set(source, 'UserData', event);
assert_istrue(__web_figure_key_event__(...
  f, true, 'a', 'A', true, false, true, false));
drawnow();
assert_isequal(f.UserData.Character, 'a');
assert_isequal(f.UserData.Key, 'A');
assert_isequal(f.UserData.Modifier, {'shift'; 'alt'});

f.WindowButtonMotionFcn = @(source, event) set(source, 'UserData', source.CurrentPoint);
[delivered, listensToMotion] = __figure_mouse_event__(...
  f, 'motion', [100 80], 'web');
drawnow();
assert_istrue(delivered);
assert_istrue(listensToMotion);
assert_isapprox(f.CurrentPoint, [100 80], 1e-12);

button = uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'Apply', ...
  'Callback', @(source, event) set(f, 'UserData', 42));
drawnow();
displayList = jsondecode(__web_display_list__(f));
controls = displayList.controls;
if iscell(controls)
  controlHandle = controls{1}.handle;
else
  controlHandle = controls(1).handle;
end
assert_istrue(__web_uicontrol_action__(controlHandle, 0, ''));
drawnow();
assert_isequal(f.UserData, 42);
assert_istrue(isgraphics(button));

close(f);
%=============================================================================
