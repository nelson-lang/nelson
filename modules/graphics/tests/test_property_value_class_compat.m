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
% The class() of a property value must match the reference numerical
% computing language. These properties used to report the wrong storage
% class (an empty double where the reference returns an empty typed array,
% an OnOffSwitchState enum where the reference returns a plain char, ...).
%=============================================================================
close all
%=============================================================================
% Empty tick labels report as char, not an empty cell.
f = figure('Visible', 'off');
ax = axes('Parent', f);
assert_isequal(class(get(ax, 'ZTickLabel')), 'char');
assert_isequal(class(get(ax, 'XTickLabel')), 'cell');
%=============================================================================
% uicontrol String reports as char when empty.
u = uicontrol('Style', 'pushbutton', 'String', '');
assert_isequal(class(get(u, 'String')), 'char');
%=============================================================================
close all
%=============================================================================
% parallelplot CoordinateVariables defaults to an empty cell.
p = parallelplot(magic(4));
assert_isequal(class(get(p, 'CoordinateVariables')), 'cell');
close all
%=============================================================================
% bar/barh Labels default to an empty string array.
b = bar(1:5);
assert_isequal(class(get(b, 'Labels')), 'string');
bh = barh(1:5);
assert_isequal(class(get(bh, 'Labels')), 'string');
close all
%=============================================================================
% uitable logical-list columns and char Enable.
t = uitable();
assert_isequal(class(get(t, 'ColumnEditable')), 'logical');
assert_isequal(class(get(t, 'ColumnSortable')), 'logical');
assert_isequal(class(get(t, 'Enable')), 'char');
close all
%=============================================================================
% fsurf ShowContours reports as a plain char.
fs = fsurf(@(x, y) x + y);
assert_isequal(class(get(fs, 'ShowContours')), 'char');
close all
%=============================================================================
% parallelplot LineStyle/MarkerStyle report as string scalars.
pp = parallelplot(magic(4));
assert_isequal(class(get(pp, 'LineStyle')), 'string');
assert_isequal(class(get(pp, 'MarkerStyle')), 'string');
close all
%=============================================================================
% annotation textbox/textarrow String defaults to an empty cell.
atb = annotation('textbox', [.2 .2 .2 .2]);
assert_isequal(class(get(atb, 'String')), 'cell');
close all
ata = annotation('textarrow');
assert_isequal(class(get(ata, 'String')), 'cell');
close all
%=============================================================================
