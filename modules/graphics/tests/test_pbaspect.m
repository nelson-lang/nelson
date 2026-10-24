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
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
figure();
pbaspect([1 2 1]);
a = pbaspect();
asserts.isequal(a, [1 2 1]);
%=============================================================================
figure();
[x, y] = meshgrid(-2:.2:2);
z = x .* exp(-x .^ 2 - y .^ 2);
surf(x, y, z)
pbaspect([2 1 1])
a = pbaspect();
asserts.isequal(a, [2 1 1]);
ax = gca();
asserts.isapprox(ax.XTick, -2:0.5:2, 1e-12);
asserts.isapprox(ax.YTick, [-2 0 2], 1e-12);
asserts.isapprox(ax.ZTick, [-0.5 0 0.5], 1e-12);
%=============================================================================
m = pbaspect('mode');
asserts.isequal(m, 'manual');
%=============================================================================
pbaspect('auto');
a = pbaspect();
asserts.isapprox(a, [1 0.89488791017342773 0.7180768895784686], 1e-12);
ax = gca();
asserts.isapprox(ax.XTick, -2:0.5:2, 1e-12);
asserts.isapprox(ax.YTick, -2:1:2, 1e-12);
asserts.isapprox(ax.ZTick, [-0.5 0 0.5], 1e-12);
m = pbaspect('mode');
asserts.isequal(m, 'auto');
%=============================================================================
figure();
X = rand(100, 1);
Y = rand(100, 1);
Z = rand(100, 1);
scatter3(X, Y, Z);
pbaspect([3 2 1]);
pbaspect('auto');
ax = gca();
asserts.isapprox(ax.XTick, 0:0.2:1, 1e-12);
asserts.isapprox(ax.YTick, 0:0.2:1, 1e-12);
asserts.isapprox(ax.ZTick, 0:0.2:1, 1e-12);
%=============================================================================
