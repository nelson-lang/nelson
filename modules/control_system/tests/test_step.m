%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
A = [-10 -20 -30; 1 0 0; 0 1 0];
B = [1; 0; 0];
C = [0 0 1];
D = 0;
T = [0:0.1:1];
U = zeros(size(T, 1), size(T, 2));
X0 = [0.1 0.1 0.1];
sys = ss(A, B, C, D);
[x, t, y] = step(sys);

asserts.isequal(size(x), [570 1]);
asserts.isequal(size(t), [570 1]);
asserts.isequal(size(y), [570 3]);
asserts.isapprox(x(1), 0, 1e-3);
asserts.isapprox(x(end), 0.0333, 1e-2);
asserts.isapprox(t(1), 0, 1e-3);
asserts.isapprox(t(end), 5.69, 1e-2);
asserts.isapprox(y(1, :), [0 0 0], 1e-3);
asserts.isapprox(y(end, :), [-0.0005 0.0001 0.0334], 1e-2);
%=============================================================================
step(sys);
%=============================================================================
% Regression: step(sys, tFinal) with a scalar final time must build a
% 0:dt:tFinal grid (it used to be treated as a ready-made vector and crashed
% in lsim on T(2)).
[xs, ts, ys] = step(sys, 3);
asserts.isapprox(ts(1), 0, 1e-12);
asserts.isapprox(ts(end), 3, 1e-12);
asserts.isequal(size(xs, 1), numel(ts));
asserts.isequal(size(ys, 1), numel(ts));
% A scalar final time and the matching [t0, tFinal] range agree.
[xr, tr] = step(sys, [0 3]);
asserts.isequal(numel(tr), numel(ts));
asserts.isapprox(tr(end), 3, 1e-12);
asserts.isapprox(xr, xs, 1e-10);
% The scalar-final-time plotting path renders without error.
f = figure('Visible', 'off');
step(sys, 3);
close(f);
%=============================================================================
% Regression: discrete-time step(sys, tFinal) with a scalar final time honours
% the sample time when building the grid.
sysd = ss(0.5, 1, 1, 0, 0.1);
[xd, td] = step(sysd, 2);
asserts.isapprox(td(1), 0, 1e-12);
asserts.isapprox(td(end), 2, 1e-12);
asserts.isapprox(td(2) - td(1), 0.1, 1e-12);
%=============================================================================
