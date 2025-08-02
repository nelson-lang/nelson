%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
numerator = [20 10];
denominator = [40 30 20];
[A, B, C, D, E] = compreal(numerator, denominator);
A_REF = -0.7500;
B_REF = 0.2500;
C_REF = -0.5000;
D_REF = 0.5000;
E_REF = [];
assert_isapprox(A, A_REF, 1e-4);
assert_isapprox(B, B_REF, 1e-4);
assert_isapprox(C, C_REF, 1e-4);
assert_isapprox(D, D_REF, 1e-4);
assert_isapprox(E, E_REF, 1e-4);
%=============================================================================
numerator = [10 10 0];
denominator = [10 20 30];
[A, B, C, D, E] = compreal(numerator, denominator);
A_REF = [-2.0000   -1.5000; 2.0000         0];
B_REF = [2;      0];
C_REF = [-0.5000   -0.7500];
D_REF =  1;
E_REF = [];
assert_isapprox(A, A_REF, 1e-4);
assert_isapprox(B, B_REF, 1e-4);
assert_isapprox(C, C_REF, 1e-4);
assert_isapprox(D, D_REF, 1e-4);
assert_isapprox(E, E_REF, 1e-4);
%=============================================================================
numerator = 10;
denominator = 10;
[A, B, C, D, E] = compreal(numerator, denominator);
A_REF = [];
B_REF = zeros(0, 1);
C_REF = zeros(1, 0);
D_REF = 1;
E_REF = [];
assert_isapprox(A, A_REF, 1e-4);
assert_isapprox(B, B_REF, 1e-4);
assert_isapprox(C, C_REF, 1e-4);
assert_isapprox(D, D_REF, 1e-4);
assert_isapprox(E, E_REF, 1e-4);
%=============================================================================
numerator = [10 10];
denominator = [10 10 100];
[A, B, C, D, E] = compreal(numerator, denominator);
A_REF = -1;
B_REF = 1;
C_REF = 0;
D_REF = 1;
E_REF = [];
assert_isapprox(A, A_REF, 1e-4);
assert_isapprox(B, B_REF, 1e-4);
assert_isapprox(C, C_REF, 1e-4);
assert_isapprox(D, D_REF, 1e-4);
assert_isapprox(E, E_REF, 1e-4);
%=============================================================================
numerator = [0 10 10];
denominator = [1 1 10];
[A, B, C, D, E] = compreal(numerator, denominator);
A_REF = [-1.0000   -2.5000; 4.0000         0];
B_REF = [4;  0];
C_REF = [2.5000    0.6250];
D_REF = 0;
E_REF = [];
assert_isapprox(A, A_REF, 1e-4);
assert_isapprox(B, B_REF, 1e-4);
assert_isapprox(C, C_REF, 1e-4);
assert_isapprox(D, D_REF, 1e-4);
assert_isapprox(E, E_REF, 1e-4);
%=============================================================================

