%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% membrane returns eigenfunctions of the L-shaped membrane on a (2n+1)-by-(2n+1)
% grid, normalised so the largest magnitude is 1 with a positive peak.
%=============================================================================
% membrane relies on eigs (shift-invert), so it needs ARPACK. Skip the whole
% suite on builds without ARPACK, exactly like test_eigs / test_svds.
try
  probeMembrane = membrane(1, 15); %#ok<NASGU> capture output: nargout>0 avoids the surf() display path
catch ex
  skip_testsuite(strcmp(ex.message, _('ARPACK support is not available.')), ex.message);
  rethrow(ex);
end
%=============================================================================
assert_isequal(size(membrane), [31 31]);
assert_isequal(size(membrane(1, 15)), [31 31]);
assert_isequal(size(membrane(1, 100)), [201 201]);
assert_isequal(size(membrane(3, 25)), [51 51]);
%=============================================================================
% The result is real, and normalised to a unit positive peak.
L = membrane(1, 20);
assert_istrue(isreal(L));
assert_isapprox(max(L(:)), 1, 1e-10);
%=============================================================================
% The first eigenfunction is the fundamental mode: no interior sign change, so
% every value is nonnegative.
assert_istrue(min(L(:)) >= -1e-9);
%=============================================================================
% A higher mode has a nodal line, hence negative values.
L3 = membrane(3, 25);
assert_istrue(min(L3(:)) < -0.05);
%=============================================================================
% The missing quadrant and the outer frame are exactly zero (Dirichlet
% boundary): a quarter of the grid plus the frame is zero.
L1 = membrane(1, 15);
assert_istrue(nnz(L1 == 0) > numel(L1) / 4);
%=============================================================================
% Stable: the eigensolve gives the same surface each call up to the iterative
% solver's convergence noise (its starting vector is not seeded).
L2 = membrane(2, 20);
for repeat = 1:6
  assert_isapprox(membrane(2, 20), L2, 1e-8, 1e-10);
end
assert_istrue(L2(2, 2) > 0);
%=============================================================================
