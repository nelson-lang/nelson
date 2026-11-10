%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = cat(3, [4 1; 2 3], [2 0; 1 5]);
S = pagesvd(A);
assert_isequal(size(S), [2 1 2]);
assert_isapprox(S(:), [5.116672736016927; 1.954395075848307; 5.116672736016927; 1.954395075848307], 1e-12);
[U, S, V] = pagesvd(A);
assert_isequal([size(U) size(S) size(V)], [2 2 2 2 2 2 2 2 2]);
for k = 1:2
  assert_isapprox(U(:, :, k) * S(:, :, k) * V(:, :, k)', A(:, :, k), 1e-12);
end
X = cat(3, [1 2; 3 4; 5 6], eye(3, 2));
S = pagesvd(X);
assert_isapprox(S(:), [9.525518091565107; 0.514300580658644; 1; 1], 1e-12);
assert_isequal(size(pagesvd(X, 'matrix')), [3 2 2]);
[U, S, V] = pagesvd(X, 'econ');
assert_isequal([size(U) size(S) size(V)], [3 2 2 2 2 2 2 2 2]);
[U, S, V] = pagesvd(X, 'vector');
assert_isequal(size(S), [2 1 2]);
assert_isequal(class(pagesvd(single(A))), 'single');
assert_checkerror('pagesvd(A, ''foo'')', _('Options must be ''econ'', ''vector'' or ''matrix''.'));
%=============================================================================
