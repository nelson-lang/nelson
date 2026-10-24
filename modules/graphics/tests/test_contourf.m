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
figure();
Z = peaks(25);
[M, h] = contourf(Z);
asserts.isequal(class(h), 'nelson.graphics.chart.primitive.Contour');
asserts.isequal(h.Type, 'contour');
asserts.isequal(h.FaceColor, 'flat');
asserts.isequal(char(h.Fill), 'on');
asserts.isapprox(h.EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
asserts.isequal(M, h.ContourMatrix);
%=============================================================================
%=============================================================================
f = figure('Visible', 'off');
x = linspace(-2 * pi, 2 * pi, 100);
y = linspace(-2 * pi, 2 * pi, 100);
[X, Y] = meshgrid(x, y);
Z = sin(X) .* cos(Y);
contourf(X, Y, Z, 20);
ax = gca();
asserts.isequal(ax.XTick, -6:2:6);
asserts.isequal(ax.YTick, -6:2:6);
close(f);
%=============================================================================