%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
M = [1 0 1; 0 1 0; 1 0 1];
R = orth(M);
% Each orthonormal basis vector is defined only up to a sign, and the sign is
% chosen per-column independently by the LAPACK backend (Apple Accelerate
% flips columns differently from Intel MKL), so compare magnitudes.
REF = [0.7071, 0; 0, 1.0000; 0.7071, 0];
asserts.isapprox(abs(R), abs(REF), 1e-4);
asserts.isapprox(R' * R, eye(2), 1e-4);
%=============================================================================
M = [1 0 1; 0 1 0; 1 0 1];
R = orth(M, 1);
REF = [0.7071; 0; 0.7071];
asserts.isapprox(abs(R), abs(REF), 1e-4);
%=============================================================================
asserts.isequal(orth([1 0; 0 0]), [1; 0]);
R = orth([1 1; 1 1]);
asserts.istrue(asserts.isapprox(R, [1; 1] / sqrt(2), 1e-12) ...
  || asserts.isapprox(R, -[1; 1] / sqrt(2), 1e-12));
%=============================================================================
