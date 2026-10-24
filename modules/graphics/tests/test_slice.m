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
[x, y, z] = meshgrid(-2:2, -2:2, -2:2);
v = x .^ 2 + y .^ 2 + z .^ 2;
f = figure('Visible', 'off');
h = slice(x, y, z, v, 0, [], 0);
asserts.isequal(numel(h), 2);
asserts.istrue(isgraphics(h(1), 'surface'));
asserts.isequal(h(1).FaceColor, 'flat');
asserts.isapprox(h(1).EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
asserts.isequal(h(1).UserData.ChartType, 'slice');
ax = ancestor(h(1), 'axes');
asserts.isequal(ax.View, [-37.5 30]);
asserts.isequal(ax.CLim, [0 12]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = slice(ax, v, 2, 3, []);
asserts.isequal(numel(h), 2);
asserts.isequal(h(1).Parent, ax);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = slice(x, y, z, v, 0, [], 0, 'nearest');
asserts.isequal(numel(h), 2);
asserts.istrue(isgraphics(h(1), 'surface'));
close(f);
%=============================================================================
f = figure('Visible', 'off');
[sx, sy] = meshgrid(-1:1, -1:1);
sz = zeros(size(sx));
h = slice(x, y, z, v, sx, sy, sz);
asserts.isequal(numel(h), 1);
asserts.istrue(isgraphics(h, 'surface'));
asserts.isequal(size(h.XData), [3 3]);
asserts.isequal(size(h.CData), [3 3]);
close(f);
%=============================================================================
asserts.checkerror('slice(ones(2), 1, 1, 1);', getString(message('Nelson:graphics:invalidVolumeData')));
asserts.checkerror('slice(ones(2,2,2), {1}, [], []);', getString(message('Nelson:validators:mustBeNumeric', 'Slice locations')));
%=============================================================================
