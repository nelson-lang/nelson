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
A = [0.9, 0.2; 0, 0.8];
B = [0; 2];
Q = [4, 0; 0, 4];
R = 3;
[K, S, e] = dlqr(A, B, Q, R);

K_REF = [0.2146    0.3964];
S_REF = [15.3852    2.7876; 2.7876    5.0951];
e_REF = [0.7904; 0.1169];

assert_isapprox(K, K_REF, 1e-4);
assert_isapprox(S, S_REF, 1e-4);
assert_isapprox(e, e_REF, 1e-4);
%=============================================================================
