%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [-10 0.1; 0 -20];
B = [0; 10];
C = [10 0];
D = [0];
G = [10 0; 0 10];
H = [0 0];
Q = [0.1 0; 0 0.1];
R = 10;
N = [0; 0];
sys = ss(A, [B G], C, [D H]);
[kEst, l, p, m, z] = kalman(sys, Q, R, N);

A_REF = [-14.14        0.1;     -0.007322        -20];
B_REF = [ 0     0.4142;         10  0.0007322];
C_REF = [10     0; 1     0; 0     1];
D_REF = [0   0; 0   0; 0   0];
l_REF = [0.4142; 0.0007];
p_REF = [0.4142    0.0007; 0.0007    0.2500];
m_REF = [];
z_REF = [];

assert_isapprox(kEst.A, A_REF, 1e-4);
assert_isapprox(kEst.B, B_REF, 1e-4);
assert_isapprox(kEst.C, C_REF, 1e-4);
assert_isapprox(kEst.D, D_REF, 1e-4);
assert_isapprox(l, l_REF, 1e-4);
assert_isapprox(p, p_REF, 1e-3);
assert_isapprox(m, m_REF, 1e-4);
assert_isapprox(z, z_REF, 1e-4);
%=============================================================================
A = [11.269   -0.4940    1.129; 1.0000         0         0;0    1.0000         0];
B = [-0.3832;  0.5919;  0.5191];
C = [1 0 0];
sys = ss(A,[B, B], C, 0);
Q = 1;
R = 1;
[kEst, l, p, m, z] = kalman(sys, Q, R, []);

A_REF = [-11.44  -0.494   1.129;
-1.162       0       0;
-2.568       1       0];
B_REF = [-0.3832    22.71;
0.5919    2.162;
0.5191    2.568];
C_REF = [1     0     0;
1     0     0;
0     1     0;
0     0     1];
D_REF = [0   0;0   0;0   0;0   0];
l_REF = [22.7058; 2.1620; 2.5678];
p_REF = [22.7058    2.1620    2.5678;
2.1620    2.6767    3.1622;
2.5678    3.1622   25.6571];
m_REF = [];
z_REF = [];

assert_isapprox(kEst.A, A_REF, 1e-3);
assert_isapprox(kEst.B, B_REF, 1e-3);
assert_isapprox(kEst.C, C_REF, 1e-4);
assert_isapprox(kEst.D, D_REF, 1e-4);
assert_isapprox(l, l_REF, 1e-4);
assert_isapprox(p, p_REF, 1e-3);
assert_isapprox(m, m_REF, 1e-4);
assert_isapprox(z, z_REF, 1e-4);
%=============================================================================
A = [1.1269   -0.4940    0.1129; 1.0000         0         0; 0    1.0000         0];
B = [-0.3832,-0.3832; 0.5919,0.5919; 0.5191, 0.5191];
C = [1 0 0];
D = 0;
sys = ss(A, B, C, 0, -1);
Q = 2.3;
R = 1;

[kEst, l, p, m, z] = kalman(sys, Q, R);

A_REF = [0.5835    -0.494    0.1129;
0.4655         0         0;
-0.01013         1         0];
B_REF = [-0.3832   0.5434;
0.5919   0.5345;
0.5191  0.01013];
C_REF = [0.4655         0         0;
0.4655         0         0;
-0.01013         1         0;
0.4776         0         1];
D_REF = [0   0.5345;
0   0.5345;
0  0.01013;
0  -0.4776];
Ts_REF = -1;
l_REF = [0.5434;
0.5345;
0.0101];
p_REF = [1.1484    0.0218   -1.0260;
0.0218    1.3403    0.7168;
-1.0260    0.7168    1.9599];
m_REF = [0.5345;
0.0101;
-0.4776];
z_REF = [0.5345    0.0101   -0.4776;
0.0101    1.3401    0.7272;
-0.4776    0.7272    1.4699];

assert_isapprox(kEst.A, A_REF, 1e-3);
assert_isapprox(kEst.B, B_REF, 1e-3);
assert_isapprox(kEst.C, C_REF, 1e-4);
assert_isapprox(kEst.D, D_REF, 1e-4);
assert_isapprox(kEst.Ts, Ts_REF, 1e-4);
assert_isapprox(l, l_REF, 1e-4);
assert_isapprox(p, p_REF, 1e-3);
assert_isapprox(m, m_REF, 1e-4);
assert_isapprox(z, z_REF, 1e-4);
%=============================================================================
