%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Linear interp2 on uniform grids (1:n or equally spaced vectors).
%=============================================================================
f = @(x, y) 2 * y + 3 * x + x .* y;
[X, Y] = meshgrid(1:7, 1:5);
V = f(X, Y);
%=============================================================================
% exact values (a bilinear function is reproduced exactly)
xq = [1.25 2.5 6.75 3.1; 1 7 4.5 5.9];
yq = [1.5 4.75 2.2 3; 5 1 4.5 2.1];
R = interp2(V, xq, yq);
asserts.isequal(size(R), size(xq));
asserts.isapprox(R, f(xq, yq), 1e-12);
%=============================================================================
% nodes are returned exactly, including the last row and column
R = interp2(V, [1 7 7 1 4], [1 1 5 5 3]);
asserts.isequal(R, [V(1, 1), V(1, 7), V(5, 7), V(5, 1), V(3, 4)]);
%=============================================================================
% out of range and NaN query points give NaN
xq = [0.999, 7 + 1e-9, 2, 2, NaN, Inf, -Inf, 3];
yq = [2, 2, 0.5, 5.5, 2, 2, 2, NaN];
R = interp2(V, xq, yq);
asserts.isequal(isnan(R), true(1, 8));
%=============================================================================
% extrapolation value for points outside, inside points unchanged
R = interp2(V, [0, 2.5, 8, 7], [2, 3.5, 2, 5], 'linear', -7);
asserts.isequal(R([1 3]), [-7 -7]);
asserts.isapprox(R([2 4]), f([2.5 7], [3.5 5]), 1e-12);
R = interp2(V, NaN, 2, 'linear', 0);
asserts.isequal(R, 0);
%=============================================================================
% equally spaced grid vectors with rounding (linspace)
x = linspace(0, 1, 11);
y = linspace(-2, 3, 9).';
[X, Y] = meshgrid(x, y);
V2 = f(X, Y);
R = interp2(x, y, V2, x, y);
asserts.isequal(R, V2);
R = interp2(x, y, V2, [0.05 0.3 0.999 1], [-2 0.1 2.9 3]);
asserts.isapprox(R, f([0.05 0.3 0.999 1], [-2 0.1 2.9 3]), 1e-12);
%=============================================================================
% nearly uniform grid: the query must fall in the actual grid cell
x = [0 1 2.05 3 4];
y = [0; 1];
V4 = [x .^ 2; x .^ 2];
R = interp2(x, y, V4, [2.02 2.06 3.99], [0 1 0.5]);
expected = [1 + (1.02 / 1.05) * (2.05 ^ 2 - 1), 2.05 ^ 2 + (0.01 / 0.95) * (9 - 2.05 ^ 2), 9 + 0.99 * 7];
asserts.isapprox(R, expected, 1e-12);
%=============================================================================
% NaN samples only contaminate their own cells
W = V;
W(2, :) = NaN;
R = interp2(W, [2.5 2.5 2.5], [3 1 2.5]);
asserts.isapprox(R(1), f(2.5, 3), 1e-12);
asserts.isequal(isnan(R(2:3)), [true true]);
%=============================================================================
% query vectors of different sizes form a grid
R = interp2(V, [1.5 2.5 3.5], [1.25; 4.5]);
[XQ, YQ] = meshgrid([1.5 2.5 3.5], [1.25; 4.5]);
asserts.isequal(size(R), [2 3]);
asserts.isapprox(R, f(XQ, YQ), 1e-12);
%=============================================================================
% decreasing uniform grid
x = 7:-1:1;
y = (5:-1:1).';
R = interp2(x, y, fliplr(flipud(V)), [2.5 6], [1.5 4]);
asserts.isapprox(R, f([2.5 6], [1.5 4]), 1e-12);
%=============================================================================
% non-uniform grid (generic path)
x = [1 2 4 8 9];
y = [0; 0.5; 3; 10];
[X, Y] = meshgrid(x, y);
V3 = f(X, Y);
xq = [1.5 3 7.25 9 1];
yq = [0.25 2 9 10 0];
R = interp2(x, y, V3, xq, yq);
asserts.isapprox(R, f(xq, yq), 1e-12);
R = interp2(x, y, V3, [0.5 5], [1 11]);
asserts.isequal(isnan(R), [true true]);
%=============================================================================
% single precision
Vs = single(V);
R = interp2(Vs, single([1.25 7 3.5]), single([1.5 5 0]));
asserts.isequal(class(R), 'single');
asserts.isapprox(R(1:2), single(f([1.25 7], [1.5 5])), 1e-5);
asserts.isequal(isnan(R(3)), true);
R = interp2(Vs, [2.5 9], [3.5 1], 'linear', 4);
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single([f(2.5, 3.5), 4]), 1e-5);
%=============================================================================
% large random query set against a hand-written bilinear evaluation
W = reshape(sin(1:128 * 256), 128, 256);
xq = 1 + mod((1:4096) * 0.618034, 1) * 255;
yq = 1 + mod((1:4096) * 0.414214, 1) * 127;
i0 = min(floor(yq), 127);
j0 = min(floor(xq), 255);
fy = yq - i0;
fx = xq - j0;
id = i0 + (j0 - 1) * 128;
ref = (1 - fy) .* (1 - fx) .* W(id) + fy .* (1 - fx) .* W(id + 1) + (1 - fy) .* fx .* W(id + 128) + fy .* fx .* W(id + 129);
R = interp2(W, xq, yq);
asserts.isapprox(R, ref, 1e-13);
%=============================================================================
% 'nearest' and 'cubic' are not affected by the linear fast path
R = interp2(V, [1.5 2.4 6.6], [1.5 4.6 2.2], 'nearest');
asserts.isequal(R, [V(1, 1), V(5, 2), V(2, 7)]);
R = interp2(V, [2.5 3.25], [2.5 3.75], 'cubic');
asserts.isapprox(R, f([2.5 3.25], [2.5 3.75]), 1e-12);
%=============================================================================
