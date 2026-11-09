%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Real double products give the written-out sums (exact for integer values),
% the shape of each case, the class of mixed operands and the operator errors.
%=============================================================================
A = [1 2 3; 4 5 6];
B = [7 8; 9 10; 11 12];
assert_isequal(A * B, [58 64; 139 154]);
assert_isequal([1 2 3] * [4; 5; 6], 32);
assert_isequal([1; 2] * [3 4], [3 4; 6 8]);
assert_isequal(magic(3) * [1; 2; 3], [28; 34; 28]);
assert_isequal(loop_mtimes(A, B), [58 64; 139 154]);
%=============================================================================
for n = [2 5 9 70]
  R = reshape(mod(1:n * (n + 1), 7) - 3, n, n + 1);
  S = reshape(mod(1:(n + 1) * 3, 5) - 2, n + 1, 3);
  assert_isequal(R * S, written_out_product(R, S));
  assert_isequal(loop_mtimes(R, S), written_out_product(R, S));
end
M = reshape(mod(1:4900, 7), 70, 70);
assert_isequal(M * M, written_out_product(M, M));
%=============================================================================
assert_isequal(class(single(A) * B), 'single');
assert_isequal(A * (B + 1i), A * B + 1i * (A * ones(3, 2)));
%=============================================================================
assert_isequal(zeros(2, 0) * zeros(0, 3), zeros(2, 3));
assert_isequal(size(zeros(0, 2) * ones(2, 3)), [0 3]);
assert_isequal(size(ones(3, 2) * zeros(2, 0)), [3 0]);
%=============================================================================
assert_checkerror('[1 2 3] * [1 2]', 'Incorrect dimensions for matrix multiplication.');
assert_checkerror('ones(2, 3) * ones(2, 3)', 'Incorrect dimensions for matrix multiplication.');
%=============================================================================
function r = loop_mtimes(a, b)
  r = [];
  for k = 1:2
    r = a * b;
  end
end
%=============================================================================
function P = written_out_product(R, S)
  P = zeros(size(R, 1), size(S, 2));
  for i = 1:size(R, 1)
    for j = 1:size(S, 2)
      for k = 1:size(R, 2)
        P(i, j) = P(i, j) + R(i, k) * S(k, j);
      end
    end
  end
end
%=============================================================================
