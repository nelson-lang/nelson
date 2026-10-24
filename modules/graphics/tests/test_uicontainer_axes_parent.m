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
% Graphics objects accept a UI container as Parent, and the container keeps
% them in Children. Companion of test_uicontainer_axes_rendering, which checks
% that they are also painted.
%=============================================================================
f = figure('Visible', 'off', 'Position', [10 10 600 400]);
g = uitabgroup('Parent', f, 'Units', 'normalized', 'Position', [0 0 1 1]);
first = uitab('Parent', g, 'Title', 'first');
second = uitab('Parent', g, 'Title', 'second');
%=============================================================================
ax = axes('Parent', first, 'Position', [0.1 0.1 0.8 0.8]);
assert_isequal(class(get(ax, 'Parent')), 'nelson.ui.container.Tab');
assert_isequal(get(ax, 'Parent'), first);
assert_isequal(numel(first.Children), 1);
assert_isequal(first.Children(1), ax);
assert_isequal(get(ax, 'Position'), [0.1 0.1 0.8 0.8]);
%=============================================================================
h = line('Parent', ax, 'XData', 1:10, 'YData', (1:10) .^ 2);
assert_isequal(get(h, 'Parent'), ax);
%=============================================================================
% SelectedTab is drivable and defaults to the first tab
%=============================================================================
assert_isequal(g.SelectedTab, first);
g.SelectedTab = second;
assert_isequal(g.SelectedTab, second);
g.SelectedTab = first;
assert_isequal(g.SelectedTab, first);
close(f);
%=============================================================================
% same for a uipanel
%=============================================================================
f = figure('Visible', 'off', 'Position', [10 10 600 400]);
p = uipanel('Parent', f, 'Units', 'normalized', 'Position', [0 0 1 1]);
ax = axes('Parent', p, 'Position', [0.2 0.2 0.6 0.6]);
assert_isequal(class(get(ax, 'Parent')), 'nelson.ui.container.Panel');
assert_isequal(get(ax, 'Parent'), p);
assert_isequal(numel(p.Children), 1);
assert_isequal(p.Children(1), ax);
assert_isequal(get(ax, 'Position'), [0.2 0.2 0.6 0.6]);
close(f);
%=============================================================================
