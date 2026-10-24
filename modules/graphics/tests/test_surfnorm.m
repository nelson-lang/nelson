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
reset(groot());
Z = reshape(1:9, 3, 3);
[Nx, Ny, Nz] = surfnorm(Z);
N = 1 / sqrt(11);
asserts.isapprox(Nx, -3 * N * ones(3, 3), 1e-12);
asserts.isapprox(Ny, -N * ones(3, 3), 1e-12);
asserts.isapprox(Nz, N * ones(3, 3), 1e-12);
%=============================================================================
NxOnly = surfnorm(Z);
[NxTwo, NyTwo] = surfnorm(Z);
asserts.isapprox(NxOnly, Nx, 1e-12);
asserts.isapprox(NxTwo, Nx, 1e-12);
asserts.isapprox(NyTwo, Ny, 1e-12);
%=============================================================================
[X, Y] = meshgrid(10:12, 20:22);
[NxGrid, NyGrid, NzGrid] = surfnorm(X, Y, Z);
asserts.isapprox(NxGrid, Nx, 1e-12);
asserts.isapprox(NyGrid, Ny, 1e-12);
asserts.isapprox(NzGrid, Nz, 1e-12);
%=============================================================================
[NxFlat, NyFlat, NzFlat] = surfnorm(zeros(3, 4));
asserts.isequal(NxFlat, zeros(3, 4));
asserts.isequal(NyFlat, zeros(3, 4));
asserts.isequal(NzFlat, ones(3, 4));
%=============================================================================
[X, Y, Z] = sphere(10);
[U, V, W] = surfnorm(X, Y, Z);
asserts.isapprox(U(6, 1), -0.9986453807384628, 1e-12);
asserts.isapprox(V(6, 1), -0.05203271595574057, 1e-12);
asserts.isapprox(W(6, 1), 0, 1e-12);
asserts.isapprox(U(6, 6), 1, 1e-12);
asserts.isapprox(V(6, 6), 0, 1e-12);
asserts.isapprox(W(6, 6), 0, 1e-12);
asserts.isapprox(U(6, 11), -0.9986453807384628, 1e-12);
asserts.isapprox(V(6, 11), 0.05203271595574057, 1e-12);
asserts.isapprox(W(6, 11), 0, 1e-12);
%=============================================================================
f = figure('Visible', 'off');
surfnorm(Z);
ax = gca();
surfaceObjects = findobj(ax, 'Type', 'surface');
lineObjects = findobj(ax, 'Type', 'line');
asserts.istrue(numel(surfaceObjects) >= 1);
asserts.istrue(numel(lineObjects) >= 1);
asserts.isequal(numel(lineObjects(1).XData), 3 * numel(Z));
asserts.isequal(lineObjects(1).Color, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 800 600]);
[X, Y, Z] = peaks(20);
surfnorm(X, Y, Z);
ax = gca();
lineObjects = findobj(ax, 'Type', 'line');
xdata = lineObjects(1).XData;
ydata = lineObjects(1).YData;
zdata = lineObjects(1).ZData;
asserts.isapprox([xdata(1), ydata(1), zdata(1)], [-3, -3, 6.6712802967174435e-05], 1e-12);
asserts.isapprox([xdata(2), ydata(2)], [-2.999973400212302, -3.000046687396305], 1e-4);
asserts.istrue(zdata(2) > 0.8 && zdata(2) < 4);
asserts.isequal(ax.XLim, [-3, 4]);
asserts.isequal(ax.XTick, -3:4);
asserts.isequal(ax.ZLim, [-10, 10]);
asserts.isequal(ax.ZTick, -10:2:10);
asserts.isapprox(ax.PlotBoxAspectRatio, [1 0.941263597824664 0.850125502234069], 1e-12);
close(f);
%=============================================================================
assertSurfnormError('surfnorm(ones(2, 3));', 'Nelson:surfnorm:zTooSmall');
assertSurfnormError('surfnorm(ones(3), ones(3));', 'Nelson:wrongNumberOfInputs:wrongNumberMatrixInputArguments');
assertSurfnormError('surfnorm(1:3, 1:3, ones(3));', 'Nelson:dimagree:yZSizeX');
assertSurfnormError('surfnorm({1, 2, 3});', 'Nelson:graphics:mustBeRealMatrix');
%=============================================================================
function assertSurfnormError(command, identifier)
  try
    eval(command);
    asserts.fail([command, ' did not fail.']);
  catch ME
    asserts.isequal(ME.identifier, identifier);
  end
end
%=============================================================================
