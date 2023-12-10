%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sys = tf(10, [1 10 20]);
R = cloop(sys, -1);
num_REF = [0 0 10];
den_REF = [1 10 30];
REF = tf(num_REF, den_REF);
assert_isequal(R, REF);
%=============================================================================
num = 1;
den = [1 10 20];
Kp = 500;
Kd = 10;
numc = [Kd Kp];
sys = tf(conv(num, numc), den);
R = cloop(sys);
num_REF = [0 10 500];
den_REF = [1 20 520];
REF = tf(num_REF, den_REF);
assert_isequal(R, REF);
%=============================================================================
num = 1;
den = [1 10 20];
Kp = 500;
Ki = 1;
Kd = 0;
numc = [Kd Kp Ki];
denc = [1 0];
sys = tf(conv(num, numc), conv(den, denc));
R = cloop(sys);
num_REF = [0 0 500 1];
den_REF = [1 10 520 1];
REF = tf(num_REF, den_REF);
assert_isequal(R, REF);
%=============================================================================
m = 1000;
b = 50;
u = 500;
A = [0 1; 0 -b/m];
B = [0; 1/m];
C = [0 1];
D = 0;
sys = ss(A, B, C, D);

R = cloop(sys, 1); 

A_REF = [0 1;0 -0.049];
B_REF = [0; 0.001];
C_REF = [0 1];
D_REF = 0;

assert_isapprox(R.A, A_REF, 1e-4);
assert_isapprox(R.B, B_REF, 1e-4);
assert_isapprox(R.C, C_REF, 1e-4);
assert_isapprox(R.D, D_REF, 1e-4);
%=============================================================================
m = 1000;
b = 50;
u = 500;
A = [0 1; 0 -b/m];
B = [0; 1/m];
C = [0 1];
D = 0;
OUTPUTS = -1;
INPUTS = 1;
sys = ss(A, B, C, D);

R = cloop(sys, OUTPUTS, INPUTS);

A_REF = [0  1;0 -0.05];
B_REF = [0;  0.001];
C_REF = [0 1];
D_REF = 0;

assert_isapprox(R.A, A_REF, 1e-4);
assert_isapprox(R.B, B_REF, 1e-4);
assert_isapprox(R.C, C_REF, 1e-4);
assert_isapprox(R.D, D_REF, 1e-4);
%=============================================================================
m = 1000;
b = 50;
u = 500;
A = [0 1; 0 -b/m];
B = [0; 1/m];
C = [0 1]
D = 0;
OUTPUTS = 0.5;
INPUTS = 1;
sys = ss(A, B, C, D);
assert_checkerror('R = cloop(sys, OUTPUTS, INPUTS);', _('index must either be real positive integers or logicals.'));
%=============================================================================
