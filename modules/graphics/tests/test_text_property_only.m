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
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'XLim', [0 1], 'YLim', [0 1]);
%=============================================================================
% property/value pairs only, 'Position' with three elements
h = text('Parent', ax, 'Position', [0.1 0.5 0.25], 'String', 'abc', 'Color', [1 1 1], 'FontSize', 12);
asserts.isequal(get(h, 'Position'), [0.1 0.5 0.25]);
asserts.isequal(get(h, 'String'), 'abc');
asserts.isequal(get(h, 'Color'), [1 1 1]);
asserts.isequal(get(h, 'FontSize'), 12);
asserts.isequal(get(h, 'Parent'), ax);
%=============================================================================
% property/value pairs only, 'Position' with two elements: z defaults to 0
h = text('Parent', ax, 'Position', [0.2 0.5], 'String', 'de');
asserts.isequal(get(h, 'Position'), [0.2 0.5 0]);
asserts.isequal(get(h, 'String'), 'de');
asserts.isequal(get(h, 'Parent'), ax);
%=============================================================================
% no 'Parent': the current axes is used
h = text('Position', [0.3 0.6 0.1], 'String', 'no parent');
asserts.isequal(get(h, 'Position'), [0.3 0.6 0.1]);
asserts.isequal(get(h, 'Parent'), gca());
%=============================================================================
% no 'Position': default position is used
h = text('String', 'only string');
asserts.isequal(get(h, 'Position'), [0 0 0]);
asserts.isequal(get(h, 'String'), 'only string');
%=============================================================================
% no argument at all: a default text object is created in the current axes
h = text();
asserts.isequal(get(h, 'Position'), [0 0 0]);
asserts.isequal(get(h, 'String'), '');
asserts.isequal(get(h, 'Parent'), gca());
asserts.istrue(isgraphics(h, 'text'));
%=============================================================================
% only the axes: a default text object is created in that axes
h = text(ax);
asserts.isequal(get(h, 'Position'), [0 0 0]);
asserts.isequal(get(h, 'String'), '');
asserts.isequal(get(h, 'Parent'), ax);
%=============================================================================
% leading axes argument followed by property/value pairs only
h = text(ax, 'Position', [0.4 0.4 0], 'String', 'ax leading');
asserts.isequal(get(h, 'Position'), [0.4 0.4 0]);
asserts.isequal(get(h, 'Parent'), ax);
%=============================================================================
% 'Parent' given after another property
h = text('Position', [0.5 0.5 0], 'Parent', ax, 'String', 'parent in middle', 'Rotation', 45);
asserts.isequal(get(h, 'Parent'), ax);
asserts.isequal(get(h, 'Rotation'), 45);
%=============================================================================
% property names are not case sensitive
h = text('parent', ax, 'position', [0.6 0.6 0], 'string', 'lowercase');
asserts.isequal(get(h, 'Position'), [0.6 0.6 0]);
asserts.isequal(get(h, 'String'), 'lowercase');
asserts.isequal(get(h, 'Parent'), ax);
%=============================================================================
% string arguments are accepted
h = text("Parent", ax, "String", "from string", "Position", [0.7 0.7 0]);
asserts.isequal(get(h, 'Position'), [0.7 0.7 0]);
asserts.isequal(get(h, 'String'), 'from string');
%=============================================================================
% a single handle is returned
asserts.isequal(size(h), [1 1]);
asserts.istrue(isgraphics(h, 'text'));
%=============================================================================
% a dangling property name is rejected
assert_checkerror('text(''String'', ''a'', ''Color'')', getString(message('Nelson:graphics:pvPairsExpected')), 'Nelson:graphics:pvPairsExpected');
%=============================================================================
% non regression: positional forms are unchanged
h = text(0.5, 0.5, 'a');
asserts.isequal(get(h, 'Position'), [0.5 0.5 0]);
asserts.isequal(get(h, 'String'), 'a');
%=============================================================================
h = text(ax, 0.4, 0.4, 'b');
asserts.isequal(get(h, 'Position'), [0.4 0.4 0]);
asserts.isequal(get(h, 'Parent'), ax);
%=============================================================================
h = text(0.3, 0.3, 0.2, 'c');
asserts.isequal(get(h, 'Position'), [0.3 0.3 0.2]);
%=============================================================================
h = text(ax, 0.2, 0.2, 0.1, 'd');
asserts.isequal(get(h, 'Position'), [0.2 0.2 0.1]);
asserts.isequal(get(h, 'Parent'), ax);
%=============================================================================
h = text([0.1 0.2], [0.9 0.8], 'multi');
asserts.isequal(size(h), [1 2]);
asserts.isequal(get(h(1), 'Position'), [0.1 0.9 0]);
asserts.isequal(get(h(2), 'Position'), [0.2 0.8 0]);
%=============================================================================
h = text(0.6, 0.6, 'e', 'Color', [1 0 0], 'FontSize', 20);
asserts.isequal(get(h, 'Color'), [1 0 0]);
asserts.isequal(get(h, 'FontSize'), 20);
%=============================================================================
h = text(0.7, 0.7, 'f', 'Parent', ax);
asserts.isequal(get(h, 'Parent'), ax);
%=============================================================================
assert_checkerror('text(1, 2)', getString(message('Nelson:narginchk:notEnoughInputs')), 'Nelson:narginchk:notEnoughInputs');
assert_checkerror('text(1, 2, 3)', getString(message('Nelson:text:invalidLastArgument')), 'Nelson:text:invalidLastArgument');
%=============================================================================
close(f);
%=============================================================================
