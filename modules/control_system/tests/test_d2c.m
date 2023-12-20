%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [0.25, 0.5; 0, 0.1];
B = [1; 0];
C = [-1, 0];
sys = ss(A, B, C, 0, 0.2);
%=============================================================================
sysc = d2c(sys, 'prewarp', 1.2);

A_REF = [-5.9712    7.2378; -0   -8.1425];
B_REF = [3.5691;       0];
C_REF = [-3.5691    1.6223];
D_REF = 0.8000;
Ts_REF = 0;

assert_isequal(sysc.Ts, Ts_REF);
assert_isapprox(sysc.A, A_REF, 1e-4);
assert_isapprox(sysc.B, B_REF, 1e-4);
assert_isapprox(sysc.C, C_REF, 1e-4);
assert_isapprox(sysc.D, D_REF, 1e-4);
%=============================================================================
sysc = d2c(sys, 'zoh');

A_REF = [-6.9315   15.2715; 0  -11.5129];
B_REF = [9.2420;       0];
C_REF = [-1     0];
D_REF = 0;

assert_isequal(sysc.Ts, Ts_REF);
assert_isapprox(sysc.A, A_REF, 1e-4);
assert_isapprox(sysc.B, B_REF, 1e-4);
assert_isapprox(sysc.C, C_REF, 1e-4);
assert_isapprox(sysc.D, D_REF, 1e-4);
%=============================================================================
sysc = d2c(sys, 'tustin');

A_REF = [-6.0000    7.2727; -0   -8.1818];
B_REF = [3.5777;    0];
C_REF = [-3.5777    1.6262];
D_REF = 0.8000;

assert_isequal(sysc.Ts, Ts_REF);
assert_isapprox(sysc.A, A_REF, 1e-4);
assert_isapprox(sysc.B, B_REF, 1e-4);
assert_isapprox(sysc.C, C_REF, 1e-4);
assert_isapprox(sysc.D, D_REF, 1e-4);
%=============================================================================
