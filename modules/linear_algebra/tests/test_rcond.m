%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('rcond'), 1);
assert_isequal(nargout('rcond'), 1);
%=============================================================================
assert_isequal(rcond([]), Inf);
assert_isequal(rcond(Inf), 0);
assert_isequal(rcond(NaN), NaN);
%=============================================================================
assert_isequal(rcond(eye(30, 30)), 1);
assert_isequal(rcond([0 0;2 3]), 0);
assert_isequal(rcond([2 NaN;2 3]), NaN);
assert_isapprox(rcond([1, 2; 3, 4]), 0.0476, 1e-3);
assert_isapprox(rcond([i, 2; i, 4]), 0.0667, 1e-3);
%=============================================================================
if ispc() && ~strcmp(computer('arch'), 'woa64')
  % mkl 
  assert_isequal(rcond([2 Inf;2 3]), NaN);
else
  % blas, openblas or apple
  R = isequaln(rcond([2 Inf;2 3]), NaN) || isequaln(rcond([2 Inf;2 3]), 0);
  assert_istrue(R);
end
assert_isequal(rcond([0 Inf;2 3]), 0);
assert_isequal(rcond([0 0;i() 3]), 0);
assert_isequal(rcond([i Inf;2 3]), 0);
assert_isequal(rcond([2 NaN;2i 3]), NaN);
assert_isequal(rcond([i Inf;2 3]), 0);
%=============================================================================
if ispc() && ~strcmp(computer('arch'), 'woa64')
  % MacOs blas/lapack ref fails for single ...
  assert_isapprox(rcond(single(eye(30, 30))), single(1), 1e-1);
  assert_isapprox(rcond(single([1, 2; 3, 4])), single(0.0476), 1e-3);
  assert_isapprox(rcond(single([i, 2; i, 4])), single(0.0667), 1e-3);
  assert_isequal(rcond(single([2 Inf;2 3])), single(NaN));
end
assert_isapprox(rcond(single([0 0;2 3])), 0, 1e-1);
assert_isequal(rcond(single([2 NaN;2 3])), single(NaN));
%=============================================================================
assert_isequal(rcond(single([0 Inf;2 3])), single(0));
assert_isequal(rcond(single([0 0;i() 3])), single(0));
assert_isequal(rcond(single([i Inf;2 3])), single(0));
assert_isequal(rcond(single([2 NaN;2i 3])), single(NaN));
assert_isequal(rcond(single([i Inf;2 3])), single(0));
%=============================================================================
assert_checkerror('rcond(zeros(3,5))', _('Square matrix expected.'));
%=============================================================================
