%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
A = [-3 0; 1 0];
B = [2; 0];
C = [0 1];
D = [0];

sys1 = ss(A, B, C, D);
sys2 = tf(1, [8 2 1]);
sys = parallel(sys1, sys2);

A_REF = [-3.0000 0 0 0;
  1.0000 0 0 0;
  0 0 -0.2500 -0.2500;
  0 0 0.5000 0];
B_REF = [2.0000; 0; 0.5000; 0];
C_REF = [0 1.0000 0 0.5000];
D_REF = 0;
REF = ss(A_REF, B_REF, C_REF, D_REF, 0);

% Converting sys2 (a tf) to state space applies a diagonal balancing step
% (LAPACK dgebal). For this system the ideal scaling is 2^-1.5, exactly
% halfway between 2^-2 and 2^-1, so the factor is a tie that LAPACK backends
% round either way: 0.25 here, 0.5 elsewhere. Each rounding is a valid,
% input-output equivalent realization, so the A,B,C,D entries are not portable
% while the transfer function is. Validate the interconnection by its I/O.
asserts.isequal(size(sys.A), [4 4]);
asserts.isapprox(sys.Ts, 0, 1e-4);

computed = tf(sys);
expected = tf(REF);
asserts.isapprox(computed.Numerator{1}, expected.Numerator{1}, 1e-6, 1e-9);
asserts.isapprox(computed.Denominator{1}, expected.Denominator{1}, 1e-6, 1e-9);
%=============================================================================
sys2 = tf(1, [8 2 1]);
sys = parallel(sys2, sys2);
asserts.isapprox(sys.Numerator{1}, [0 0 16 4 2]);
asserts.isapprox(sys.Denominator{1}, [64 32 20 4 1]);
%=============================================================================
sysd = tf(1, [1 -0.5], 0.2);
sys = parallel(tf(2), sysd);
asserts.isequal(sys.Ts, 0.2);
%=============================================================================
