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
close all
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
l = __line__('Parent', ax, 'XData', 1:3, 'YData', 4:6, 'Color', [1 0 0], 'LineWidth', 3);
c = copyobj(l, ax);
asserts.istrue(isgraphics(c, 'line'));
asserts.isfalse(c == l);
asserts.isequal(c.Parent, ax);
asserts.isequal(c.Color, [1 0 0]);
asserts.isequal(c.XData, l.XData);
asserts.isequal(numel(ax.Children), 2);
c.XData = [7 8 9];
asserts.isequal(l.XData, [1 2 3]);
%=============================================================================
c.LineWidth = 9;
reset(c);
asserts.isequal(c.LineWidth, 0.5);
s = get(c);
asserts.istrue(isstruct(s));
asserts.istrue(isfield(s, 'Type'));
asserts.isequal(s.Type, 'line');
reset(groot());
%=============================================================================
close all
%=============================================================================
f = figure('Visible', 'off');
ax = polaraxes('Parent', f);
polarplot(ax, 0:0.1:1, 0:0.1:1);
copy = copyobj(ax, f);
asserts.istrue(isgraphics(copy, 'polaraxes'));
asserts.isfalse(isgraphics(copy, 'axes'));
asserts.isequal(copy.Type, 'polaraxes');
copy.Color = [0.5 0.5 0.5];
reset(copy);
asserts.istrue(isgraphics(copy, 'polaraxes'));
asserts.isequal(copy.Type, 'polaraxes');
asserts.isequal(copy.Color, [1 1 1]);
close(f);
%=============================================================================
