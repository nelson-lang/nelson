%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
skip_if_portable_graphics_backend('native mouse event synthesis is not available.');
% A click only reaches objects of the selected tab: the axes of a hidden tab
% are never picked, and a click never changes SelectedTab.
%=============================================================================
f = figure('Position', [100 100 400 300]);
f.WindowButtonDownFcn = @(src, evt) 1;
g = uitabgroup(f, 'Units', 'normalized', 'Position', [0 0 1 1]);
hiddenTab = uitab(g, 'Title', 'Hidden');
hiddenAxes = axes('Parent', hiddenTab, 'Position', [0 0 1 1]);
hiddenLine = line(hiddenAxes, [0 1], [0.5 0.5], 'LineWidth', 40);
shownTab = uitab(g, 'Title', 'Shown');
shownAxes = axes('Parent', shownTab, 'Position', [0.5 0.1 0.4 0.4]);
g.SelectedTab = shownTab;
drawnow();
hiddenPoint = hiddenAxes.CurrentPoint;
%=============================================================================
% empty area of the shown tab, over the hidden axes and its line
assert_istrue(__figure_mouse_event__(f, 'press', [100 140]));
__figure_mouse_event__(f, 'release', [100 140]);
drawnow();
co = f.CurrentObject;
assert_istrue(isempty(co) || ~isequal(ancestor(co, 'uitab'), hiddenTab));
assert_istrue(~isequal(f.CurrentAxes, hiddenAxes));
asserts.isequal(g.SelectedTab, shownTab);
asserts.isequal(hiddenAxes.CurrentPoint, hiddenPoint);
%=============================================================================
% inside the axes of the shown tab: it becomes the current axes
__figure_mouse_event__(f, 'press', [280 70]);
__figure_mouse_event__(f, 'release', [280 70]);
drawnow();
asserts.isequal(ancestor(f.CurrentObject, 'axes'), shownAxes);
asserts.isequal(f.CurrentAxes, shownAxes);
asserts.isequal(g.SelectedTab, shownTab);
%=============================================================================
% once selected, the other tab receives the clicks
g.SelectedTab = hiddenTab;
drawnow();
__figure_mouse_event__(f, 'press', [100 140]);
__figure_mouse_event__(f, 'release', [100 140]);
drawnow();
asserts.isequal(ancestor(f.CurrentObject, 'axes'), hiddenAxes);
asserts.isequal(g.SelectedTab, hiddenTab);
close(f);
%=============================================================================
% a drag rotates a 3D axes of the selected tab, even over a non-interactive
% axes of a hidden tab
f = figure('Position', [100 100 400 300]);
g = uitabgroup(f, 'Units', 'normalized', 'Position', [0 0 1 1]);
hiddenTab = uitab(g, 'Title', 'Hidden');
hiddenAxes = axes('Parent', hiddenTab, 'Position', [0 0 1 1]);
disableDefaultInteractivity(hiddenAxes);
shownTab = uitab(g, 'Title', 'Shown');
cascade = axes('Parent', shownTab, 'Position', [0.1 0.1 0.8 0.8]);
surf(cascade, peaks(10));
view(cascade, -35, 35);
g.SelectedTab = shownTab;
drawnow();
before = cascade.View;
__figure_mouse_event__(f, 'press', [200 130]);
__figure_mouse_event__(f, 'motion', [240 150]);
__figure_mouse_event__(f, 'release', [260 160]);
drawnow();
pause(0.5);
drawnow();
assert_isfalse(isequal(cascade.View, before));
asserts.isequal(g.SelectedTab, shownTab);
close(f);
%=============================================================================
