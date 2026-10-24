%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
figure();
x = linspace(-2 * pi, 2 * pi);
y = linspace(0, 4 * pi);
[X, Y] = meshgrid(x, y);
Z = sin(X) + cos(Y);
contour(X, Y, Z);
ax = gca();
drawnow();
asserts.isequal(ax.YTick, [0 2 4 6 8 10 12]);
%=============================================================================
% a logical Z (for example a mask from a comparison) is contoured as 0/1
f = figure('Visible', 'off');
[xm, ym] = meshgrid(-2:0.1:2, -2:0.1:2);
zmask = xm .^ 2 + ym .^ 2 < 2;
asserts.isequal(class(zmask), 'logical');
[~, h] = contour(xm, ym, zmask);
asserts.istrue(isgraphics(h, 'contour'));
asserts.isequal(h.ZData, double(zmask));
close(f);
%=============================================================================
