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
asserts.isequal(nargin('rcond'), 1);
asserts.isequal(nargout('rcond'), 1);
%=============================================================================
asserts.isequal(rcond([]), Inf);
asserts.isequal(rcond(Inf), 0);
asserts.isequal(rcond(NaN), NaN);
%=============================================================================
asserts.isequal(rcond(eye(30, 30)), 1);
asserts.isequal(rcond([0 0; 2 3]), 0);
asserts.isequal(rcond([2 NaN; 2 3]), NaN);
asserts.isapprox(rcond([1, 2; 3, 4]), 0.0476, 1e-3);
asserts.isapprox(rcond([i, 2; i, 4]), 0.0667, 1e-3);
%=============================================================================
if ispc() && ~strcmp(computer('arch'), 'woa64')
  % mkl
  asserts.isequal(rcond([2 Inf; 2 3]), NaN);
else
  % blas, openblas or apple
  R = isequaln(rcond([2 Inf; 2 3]), NaN) || isequaln(rcond([2 Inf; 2 3]), 0);
  asserts.istrue(R);
end
asserts.isequal(rcond([0 Inf; 2 3]), 0);
asserts.isequal(rcond([0 0; i() 3]), 0);
asserts.isequal(rcond([i Inf; 2 3]), 0);
asserts.isequal(rcond([2 NaN; 2i 3]), NaN);
asserts.isequal(rcond([i Inf; 2 3]), 0);
%=============================================================================
if ispc() && ~strcmp(computer('arch'), 'woa64')
  % MacOs blas/lapack ref fails for single ...
  asserts.isapprox(rcond(single(eye(30, 30))), single(1), 1e-1);
  asserts.isapprox(rcond(single([1, 2; 3, 4])), single(0.0476), 1e-3);
  asserts.isapprox(rcond(single([i, 2; i, 4])), single(0.0667), 1e-3);
  asserts.isequal(rcond(single([2 Inf; 2 3])), single(NaN));
end
asserts.isapprox(rcond(single([0 0; 2 3])), 0, 1e-1);
% Single-precision rcond of a matrix holding NaN returns NaN with MKL but 0
% with Apple/OpenBLAS LAPACK (sgecon propagates the NaN differently than the
% double-precision dgecon above); accept either.
R = isequaln(rcond(single([2 NaN; 2 3])), single(NaN)) || isequaln(rcond(single([2 NaN; 2 3])), single(0));
asserts.istrue(R);
%=============================================================================
asserts.isequal(rcond(single([0 Inf; 2 3])), single(0));
asserts.isequal(rcond(single([0 0; i() 3])), single(0));
asserts.isequal(rcond(single([i Inf; 2 3])), single(0));
% See the NaN note above: single-precision rcond of a NaN-bearing complex
% matrix yields NaN with MKL but 0 with Apple/OpenBLAS LAPACK; accept either.
R = isequaln(rcond(single([2 NaN; 2i 3])), single(NaN)) || isequaln(rcond(single([2 NaN; 2i 3])), single(0));
asserts.istrue(R);
asserts.isequal(rcond(single([i Inf; 2 3])), single(0));
%=============================================================================
asserts.checkerror('rcond(zeros(3,5))', sprintf(_('%s expected.'), 'Square matrix'));
%=============================================================================
B = [1 2; 3 4];
asserts.isapprox(rcond(B), 0.0476, 1e-3);
asserts.isequal(rcond([]), Inf);
asserts.istrue(isnan(rcond(NaN)));
asserts.checkerror('rcond(zeros(3,5))', sprintf(_('%s expected.'), 'Square matrix'));
%=============================================================================
