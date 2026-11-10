%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
cases = {[1 2; 3 4; 5 6; 7 8], [1 0; 1 1], [0.8053281314 11.10636964], [0.627222 0; 0 0.995971; 0 0; 0 0], [0.77884 0; 0 0.0896757], [4 4 2 2 2 2 4 2 2 2], [4 2 2 2 2 2 2 2 2 2];
  [1 2 3; 4 5 6], [1 0 1; 0 1 1; 2 1 0; 1 1 1], [0 0.4319328607 4.010000084], [0 0.396525 0; 0 0 0.970285], [1 0 0; 0 0.918024 0; 0 0 0.241966; 0 0 0], [2 2 4 4 3 3 2 3 4 3], [2 2 4 3 3 3 2 3 3 3];
  [1 2 3 4], [4 3 2 1; 1 1 1 1], [0 5.099019514], [0 0.981307], [1 0; 0 0.19245], [1 1 2 2 4 2 1 2 2 2], [1 1 2 2 4 2 1 2 2 2];
  [1 2 3; 4 5 6; 7 8 10], [1 1 1], [2.121320344 Inf Inf], [0.904534 0 0; 0 1 0; 0 0 1], [0.426401 0 0], [3 3 1 1 3 3 3 3 1 3], [3 3 1 1 3 3 3 3 1 3]};
for k = 1:size(cases, 1)
  [A, B, sigmaRef, cRef, sRef, sizes, econSizes] = cases{k, :};
  sigma = gsvd(A, B);
  assert_isequal(size(sigma), [numel(sigmaRef), 1]);
  assert_isequal(isinf(sigma'), isinf(sigmaRef));
  finite = ~isinf(sigmaRef);
  assert_isapprox(sigma(finite)', sigmaRef(finite), 1e-9);
  [U, V, X, C, S] = gsvd(A, B);
  assert_isequal([size(U) size(V) size(X) size(C) size(S)], sizes);
  assert_isapprox(C, cRef, 1e-5);
  assert_isapprox(S, sRef, 1e-5);
  assert_istrue(norm(U * C * X' - A) <= 1e-12 * norm(A));
  assert_istrue(norm(V * S * X' - B) <= 1e-12 * norm(B));
  assert_istrue(norm(C' * C + S' * S - eye(size(C, 2))) <= 1e-12);
  assert_istrue(norm(U' * U - eye(size(U, 2))) <= 1e-12);
  assert_istrue(norm(V' * V - eye(size(V, 2))) <= 1e-12);
  [U, V, X, C, S] = gsvd(A, B, 'econ');
  assert_isequal([size(U) size(V) size(X) size(C) size(S)], econSizes);
  assert_istrue(norm(U * C * X' - A) <= 1e-12 * norm(A));
  assert_istrue(norm(V * S * X' - B) <= 1e-12 * norm(B));
end
A = magic(4);
B = [1 2 3 4; 5 6 7 8; 2 1 0 1; 3 3 1 2];
sigma = gsvd(A, B);
assert_isapprox(sigma(2:4)', [1.980564777 8.769745364 25.6164045], 1e-9);
assert_istrue(abs(sigma(1)) < 1e-14);
[U, V, X, C, S] = gsvd(A, B);
assert_istrue(norm(U * C * X' - A) <= 1e-12 * norm(A));
assert_isequal(gsvd([1 0; 0 0], [0 0; 0 0]), Inf);
assert_isequal(class(gsvd(single(magic(3)), eye(3))), 'single');
assert_checkerror('gsvd([1 2], [1 2 3])', _('The two inputs must have the same number of columns.'));
assert_checkerror('gsvd(eye(2), eye(2), 1)', _('Option must be 0 or ''econ''.'));
%=============================================================================
