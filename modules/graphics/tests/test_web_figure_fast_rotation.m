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
% Reproduces the native-desktop rotation gesture protocol: the first and last
% frames carry a historyGroup and go through the __web_figure_action__ macro
% (history + state), while the intermediate frames take the lean C++ fast path
% (__web_display_list__ / nlsWebGuiSetAxesView) with no history. The two paths
% must set the axes View identically, and a single undo must restore the
% pre-gesture view (the whole gesture coalesces into one history entry).
%=============================================================================
f = figure('Visible', 'off');
surf(peaks(30));
axis('off');
handle = __web_graphics_handle__(f, 'number');
ax = gca();
axesHandle = __web_graphics_handle__(ax, 'number');
view(ax, -37.5, 30);
drawnow();
before = ax.View;

group = 'rotation-gesture';

% First frame of the gesture: macro path, records the pre-drag view for undo.
first = struct('action', 'view', 'axesHandle', axesHandle, ...
  'azimuth', -30, 'elevation', 32, 'historyGroup', group);
__web_figure_action__(handle, jsonencode(first), false);
asserts.isequal(ax.View, [-30 32]);

% Intermediate frames: C++ fast path, same function the macro uses, no history.
__web_display_list__(f, ax, -20, 36, false);
__web_display_list__(f, ax, -5, 44, false);
asserts.isequal(ax.View, [-5 44]);

% Final frame on release: macro path again, same group, coalesces the gesture.
last = struct('action', 'view', 'axesHandle', axesHandle, ...
  'azimuth', 12, 'elevation', 50, 'historyGroup', group);
reply = jsondecode(__web_figure_action__(handle, jsonencode(last), false));
asserts.isequal(ax.View, [12 50]);
asserts.istrue(reply.state.canUndo);

% One undo returns to the pre-gesture view: the intermediate fast-path frames
% added no stray history entries, so the gesture is a single undoable step.
__web_figure_action__(handle, '{"action":"undo"}');
asserts.isapprox(ax.View, before, 1e-10);

% Redo restores the final view of the gesture.
__web_figure_action__(handle, '{"action":"redo"}');
asserts.isequal(ax.View, [12 50]);

close(f);
%=============================================================================
