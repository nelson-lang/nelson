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
% ToolBar 'none' flags the web display list so the figure panel hides its
% navigation tools and details bar, as the native window does. The axes stay
% navigable with the mouse and the keyboard.
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:5);
drawnow();
frame = jsondecode(__web_display_list__(f));
assert_isfalse(isfield(frame, 'toolbarHidden'));

f.ToolBar = 'none';
drawnow();
frame = jsondecode(__web_display_list__(f));
assert_istrue(frame.toolbarHidden);
assert_isequal(numel(frame.rotatableAxes), 1);

f.ToolBar = 'auto';
drawnow();
frame = jsondecode(__web_display_list__(f));
assert_isfalse(isfield(frame, 'toolbarHidden'));
close(f);
%=============================================================================
