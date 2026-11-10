%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Fs = 5;
dt = 1 / Fs;
A = [cos(dt) sin(dt); -sin(dt) cos(dt)];
B = [1 - cos(dt); sin(dt)];
C = [-1 0];
D = 1;
[b, a] = ss2tf(A, B, C, D);
b_REF = [1.0000 -1.9801 0.9801];
a_REF = [1.0000 -1.9601 1.0000];
asserts.isapprox(a, a_REF, 1e-4);
asserts.isapprox(b, b_REF, 1e-4);
%=============================================================================
Fs = 16;
dt = 1 / Fs;
Ac = [0 1 0 0; -2 0 1 0; 0 0 0 1; 1 0 -2 0];
A = expm(Ac * dt);
Bc = [0 0; 1 0; 0 0; 0 1];
B = Ac \ (A - eye(4)) * Bc;
C = [-2 0 1 0; 1 0 -2 0];
D = eye(2);
[b, a] = ss2tf(A, B, C, D, 1);

b_REF = [1.0000 -3.9883 5.9727 -3.9805 0.9961;
  0 0.0020 -0.0020 -0.0020 0.0020];
a_REF = [1.0000 -3.9844 5.9688 -3.9844 1.0000];
asserts.isapprox(a, a_REF, 1e-4);
asserts.isapprox(b, b_REF, 1e-4);
%=============================================================================
[b, a] = ss2tf(A, B, C, D, 2);
b_REF = [0 0.0020 -0.0020 -0.0020 0.0020;
  1.0000 -3.9883 5.9727 -3.9805 0.9961];
a_REF = [1.0000 -3.9844 5.9688 -3.9844 1.0000];
asserts.isapprox(a, a_REF, 1e-4);
asserts.isapprox(b, b_REF, 1e-4);
%=============================================================================
% Leading zeros are kept and structural zeros stay exact
[b, a] = ss2tf([0 1; -2 -3], [0; 1], [1 0], 0);
asserts.isequal(b, [0 0 1]);
asserts.isequal(a, [1 3 2]);
%=============================================================================
[b, a] = ss2tf([0 1; -2 -3], [0; 1], [1 0; 0 1; 1 1], [0; 0; 0]);
asserts.isequal(size(b), [3 3]);
asserts.isequal(b(1:2, :), [0 0 1; 0 1 0]);
asserts.isapprox(b(3, :), [0 1 1], 1e-12);
asserts.isequal(a, [1 3 2]);
%=============================================================================
[b, a] = ss2tf([-2 1 0 0; 0 -3 1 0; 0 0 -4 1; 1 0 0 -5], [0; 0; 0; 1], [1 0 0 0], 0);
asserts.isequal(b, [0 0 0 0 1]);
asserts.isapprox(a, [1 14 71 154 119], 1e-12);
%=============================================================================
% Nonzero feedthrough
[b, a] = ss2tf([0 1; -2 -3], [0; 1], [1 1], 0.5);
asserts.isapprox(b, [0.5 2.5 2], 1e-12);
[b, a] = ss2tf([-1 0; 0 -2], [1; 1], [0 0], 3);
asserts.isapprox(b, [3 9 6], 1e-12);
%=============================================================================
% Complex zeros give real coefficients
[b, a] = ss2tf([0 1 0; 0 0 1; -6 -11 -6], [0; 0; 1], [1 0 1], 0);
asserts.istrue(isreal(b));
asserts.isapprox(b, [0 1 0 1], 1e-12);
asserts.isapprox(a, [1 6 11 6], 1e-12);
%=============================================================================
% Zero input column gives a zero numerator
[b, a] = ss2tf([1 2; 3 4], [0; 0], [1 1], 0);
asserts.isequal(b, [0 0 0]);
%=============================================================================
% Second input of a two input model
[b, a] = ss2tf([0 1; -2 -3], [0 1; 1 0], [1 0], [0 1], 2);
asserts.isapprox(b, [1 4 5], 1e-12);
%=============================================================================
% Static gain without states takes its shape from D
[b, a] = ss2tf([], [], [], 5);
asserts.isequal(b, 5);
asserts.isequal(a, 1);
[b, a] = ss2tf([], [], [], [5; 6]);
asserts.isequal(b, [5; 6]);
asserts.isequal(a, 1);
[b, a] = ss2tf([], [], [], [1 2; 3 4], 2);
asserts.isequal(b, [2; 4]);
asserts.isequal(a, 1);
[b, a] = ss2tf([], zeros(0, 2), zeros(1, 0), [1 2], 2);
asserts.isequal(b, 2);
asserts.isequal(a, 1);
[b, a] = ss2tf([], [], [], []);
asserts.isequal(b, []);
asserts.isequal(a, 1);
asserts.isequal(size(b), [0 0]);
%=============================================================================
asserts.checkerror('ss2tf([], [], [], [1 2; 3 4])', sprintf(_('%s expected.'), 'Single input system'));
%=============================================================================
