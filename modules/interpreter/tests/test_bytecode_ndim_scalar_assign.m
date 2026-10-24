%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% In-place N-dimensional scalar indexed assignment (fast path) must match the
% generic result and fall back correctly for growth / type promotion.
%=============================================================================
% 3-D scalar assignment in a loop equals the ind2sub reference
A = zeros(4, 5, 3);
for i = 1:4
  for j = 1:5
    for k = 1:3
      A(i, j, k) = i * 100 + j * 10 + k;
    end
  end
end
R = zeros(4, 5, 3);
for idx = 1:numel(R)
  [i, j, k] = ind2sub(size(R), idx);
  R(i, j, k) = i * 100 + j * 10 + k;
end
assert_isequal(A, R);
%=============================================================================
% 4-D scalar assignment
B = zeros(2, 3, 2, 2);
B(2, 3, 1, 2) = 7;
assert_isequal(B(2, 3, 1, 2), 7);
assert_isequal(sum(B(:)), 7);
%=============================================================================
% under-rank scalar indexing on an N-D array matches the collapsed layout
C = reshape(1:24, 2, 3, 4);
assert_isequal(C(2, 3), C(2, 3, 1));
D = C;
D(2, 3) = -5;
assert_isequal(D(2, 3, 1), -5);
%=============================================================================
% assignment beyond the current size grows the array (fallback)
E = zeros(2, 2, 2);
E(1, 1, 3) = 9;
assert_isequal(size(E), [2 2 3]);
assert_isequal(E(1, 1, 3), 9);
%=============================================================================
% a complex value promotes a real array (fallback)
F = zeros(2, 2, 2);
F(1, 1, 1) = 2 + 3i;
assert_isequal(F(1, 1, 1), 2 + 3i);
assert_isequal(isreal(F), false);
%=============================================================================
% a zero subscript is an error
assert_checkerror('G = zeros(2, 2, 2); G(1, 1, 0) = 1;', _('Zero or negative index encountered.'), 'Nelson:types:invalidIndex:zeroNegativeIndexEncountered');
%=============================================================================
