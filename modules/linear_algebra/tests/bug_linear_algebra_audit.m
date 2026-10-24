%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% linear_algebra audit fixes: eig/svd with NaN, trace non square,
% logm(0), expm/sqrtm on defective matrices, mpower warning
%=============================================================================
% eig with NaN input returns NaN eigenvalues (was LAPACK garbage)
asserts.isequal(eig([NaN 1; 1 1]), [NaN; NaN]);
asserts.isequal(eig([NaN 2; 1 1]), [NaN; NaN]);
[V, D] = eig([NaN 1; 1 1]);
asserts.isequal(V, [NaN NaN; NaN NaN]);
asserts.isequal(D, [NaN 0; 0 NaN]);
d = eig(single([NaN 2; 1 1]));
asserts.isequal(d, single([NaN; NaN]));
asserts.isequal(class(d), 'single');
%=============================================================================
% svd with NaN or Inf returns NaN results (was an error)
asserts.isequal(svd([NaN 1; 1 1]), [NaN; NaN]);
asserts.isequal(svd([Inf 1; 1 1]), [NaN; NaN]);
[U, S, V] = svd([NaN 1; 1 1]);
asserts.isequal(U, [NaN NaN; NaN NaN]);
asserts.isequal(S, [NaN 0; 0 NaN]);
asserts.isequal(V, [NaN NaN; NaN NaN]);
s = svd(single([NaN 1; 1 1]));
asserts.isequal(class(s), 'single');
asserts.isequal(s, single([NaN; NaN]));
s = svd([NaN 1; 1 1; 0 2]);
asserts.isequal(s, [NaN; NaN]);
%=============================================================================
% trace requires a square matrix
asserts.checkerror('trace([1 2 3; 4 5 6])', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('trace([1 2 3])', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('trace(zeros(0, 3))', sprintf(_('%s expected.'), 'Square matrix'));
% the square-matrix condition also carries a stable identifier
asserts.checkerror('trace([1 2 3; 4 5 6])', sprintf(_('%s expected.'), 'Square matrix'), 'Nelson:linear_algebra:squareMatrixExpected');
asserts.isequal(trace([]), 0);
asserts.isequal(trace(magic(3)), 15);
%=============================================================================
% logm scalar cases (logm(0) returned NaN+NaNi)
asserts.isequal(logm(0), -Inf);
asserts.isequal(logm(1), 0);
asserts.isapprox(logm(exp(2)), 2, 1e-12);
asserts.isapprox(imag(logm(-1)), pi, 1e-12);
asserts.isapprox(imag(logm(single(-1))), single(pi), single(1e-6));
lastwarn('');
r = logm(0);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:logm:nonPosRealEig');
%=============================================================================
% logm on a defective (non-diagonalizable) matrix
asserts.isapprox(logm([1 1; 0 1]), [0 1; 0 0], 1e-10);
%=============================================================================
% expm on a defective but non-singular matrix (Eigen scaling-and-squaring
% handles the non-diagonalizable case correctly).
asserts.isapprox(expm([2 1; 0 2]), exp(2) * [1 1; 0 1], 1e-10);
%=============================================================================
% expm still exact on diagonal / scalar paths
asserts.isapprox(expm([1 0; 0 2]), [exp(1) 0; 0 exp(2)], 1e-12);
asserts.isequal(expm(0), 1);
%=============================================================================
% sqrtm on defective matrices via Schur (was diagonalization garbage)
S = sqrtm([2 1; 0 2]);
asserts.isapprox(S * S, [2 1; 0 2], 1e-10);
lastwarn('');
R = sqrtm([0 1; 0 0]);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:sqrtm:SingularMatrix');
% a singular defective matrix has no valid square root: the result must be
% non-finite (previously it was silently a wrong finite matrix). The exact
% non-finite pattern (NaN vs Inf) is not asserted.
asserts.istrue(any(~isfinite(R(:))));
%=============================================================================
% mpower with non-integer exponent on a defective matrix warns
lastwarn('');
r = [2 1; 0 2] ^ 0.5;
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:mpower:IllConditionedEigenvectors');
%=============================================================================
% well conditioned mpower does not warn
lastwarn('');
r = [2 0; 0 3] ^ 0.5;
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isapprox(r, [sqrt(2) 0; 0 sqrt(3)], 1e-12);
%=============================================================================
