%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
b = [0 2 3; 1 2 1];
a = [1 0.4 1];
[A, B, C, D] = tf2ss(b, a);

A_REF = [-0.4000   -1.0000; 1.0000         0];
B_REF = [1;     0];
C_REF = [2.0000    3.0000;   1.6000         0];
D_REF = [0;      1];

assert_isapprox(A, A_REF, 1e-4);
assert_isapprox(B, B_REF, 1e-4);
assert_isapprox(C, C_REF, 1e-4);
assert_isapprox(D, D_REF, 1e-4);
%=============================================================================
[A, B, C, D] = tf2ss([], []);
assert_isequal(A, []);
assert_isequal(B, []);
assert_isequal(C, []);
assert_isequal(D, []);
%=============================================================================
Fs = 5;
dt = 1/Fs;
bf = [1 -(1+cos(dt)) cos(dt)];
af = [1 -2*cos(dt) 1];
[A, B, C, D] = tf2ss(bf, af);

A_REF = [1.9601   -1.0000; 1.0000         0];
B_REF = [1;      0];
C_REF = [-0.0199   -0.0199];
D_REF = 1;
assert_isapprox(A, A_REF, 1e-4);
assert_isapprox(B, B_REF, 1e-4);
assert_isapprox(C, C_REF, 1e-2);
assert_isapprox(D, D_REF, 1e-4);
%=============================================================================
