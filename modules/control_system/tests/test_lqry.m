%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~ismodule('slicot'), 'SLICOT is not installed');
%=============================================================================
A = [0.6, 0.25; 0, 0.9];
B = [0; 10];
C = [11, 0];
D = 0;
Q = 23;
R = 1;

[K, S, e] = lqry(A, B, C, D, Q, R);

K_REF = [56.8051 1.7777];
S_REF = [369.8481 5.6805; 5.6805 0.1778];
e_REF = [-8.1385 - 8.1025i; -8.1385 + 8.1025i];

asserts.isapprox(K, K_REF, 1e-4);
asserts.isapprox(S, S_REF, 1e-4);
asserts.isapprox(e, e_REF, 1e-4);
%=============================================================================
sys = ss(A, B, C, D);
Q = 2;
R = 1;
[K, S, e] = lqry(A, B, C, D, Q, R);

K_REF = [17.8345 1.0386];
S_REF = [63.3901 1.7834; 1.7834 0.1039];
e_REF = [-4.4430 - 4.3766i; -4.4430 + 4.3766i];

asserts.isapprox(K, K_REF, 1e-4);
asserts.isapprox(S, S_REF, 1e-4);
asserts.isapprox(e, e_REF, 1e-4);
%=============================================================================
% An indefinite weighting matrix warns and the regulator is still computed.
sys = ss([-1 1; 0 -2], [0; 1], [1 0], 0);
lastwarn('');
[K, S, e] = lqry(sys, 1, 1, 5);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:control_system:notPositiveSemiDefinite');
asserts.isapprox(K, [3.763932022500212, -0.763932022500211], 1e-10);
lastwarn('');
[K, S, e] = lqry(sys, 2, 1, 0.5);
asserts.isequal(lastwarn(), '');
asserts.isapprox(K, [0.720879176686116, 0.107547948060075], 1e-10);
%=============================================================================
