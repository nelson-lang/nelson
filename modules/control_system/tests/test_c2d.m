%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~ismodule('slicot'), 'SLICOT is not installed');
%=============================================================================
A = [1  0.5; 0.5  1 ];
B = [0 -1; 1  0 ];
C = [ -1  0; 0  1 ];
D = [  1  0; 0 -1 ];
sys = ss(A, B, C, D);
Ts = 2;
%=============================================================================
[P, G] = c2d(A, B, Ts);

P_REF = [11.4019    8.6836; 8.6836   11.4019];
G_REF = [4.6436   -8.0801;  8.0801   -4.6436];

assert_isapprox(P, P_REF, 1e-4);
assert_isapprox(G, G_REF, 1e-4);
%=============================================================================
sysd = c2d(sys, Ts, 'zoh');

Ts_REF = 2;
A_REF = [11.4019    8.6836; 8.6836   11.4019];
B_REF = [4.6436   -8.0801;  8.0801   -4.6436];
C_REF = [-1     0; 0     1];
D_REF = [1     0; 0    -1];

assert_isapprox(sysd.Ts, Ts_REF, 1e-4);
assert_isapprox(sysd.A, A_REF, 1e-4);
assert_isapprox(sysd.B, B_REF, 1e-4);
assert_isapprox(sysd.C, C_REF, 1e-4);
assert_isapprox(sysd.D, D_REF, 1e-4);
%=============================================================================
sysd = c2d(sys, Ts, 'tustin');

Ts_REF = 2;
A_REF = [-1    -4; -4    -1];
B_REF = [-2.8284        -0;  -0    2.8284];
C_REF = [-0    2.8284; -2.8284         0];
D_REF = [3     0; 0     1];

assert_isapprox(sysd.Ts, Ts_REF, 1e-4);
assert_isapprox(sysd.A, A_REF, 1e-4);
assert_isapprox(sysd.B, B_REF, 1e-4);
assert_isapprox(sysd.C, C_REF, 1e-4);
assert_isapprox(sysd.D, D_REF, 1e-4);
%=============================================================================
sysd = c2d (sys, Ts, 'prewarp', 1.5);

Ts_REF = 2;
A_REF = [-1.3466    0.1939; 0.1939   -1.3466];
B_REF = [0.4204    0.7514; -0.7514   -0.4204];
C_REF = [0.7514   -0.4204; 0.4204   -0.7514];
D_REF = [0.0885   -1.6290; -1.6290   -1.9115];

assert_isapprox(sysd.Ts, Ts_REF, 1e-4);
assert_isapprox(sysd.A, A_REF, 1e-4);
assert_isapprox(sysd.B, B_REF, 1e-4);
assert_isapprox(sysd.C, C_REF, 1e-4);
assert_isapprox(sysd.D, D_REF, 1e-4);
%=============================================================================
