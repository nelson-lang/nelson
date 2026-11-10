%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% int64 with a double scalar: values below 2^53 (fast path), above 2^53 (exact path),
% the same work done through double, and int32 for reference
N = 1e7;
X = floor(rand(1, N) * 1e12) - 5e11;
I64 = int64(X);
B64 = I64 * int64(100000) + int64(7);
U64 = uint64(abs(X));
I32 = int32(X / 1e4);
D = X;
nb = 5;
labels = {'int64 .* 2.5', 'int64 ./ 3', '3 ./ int64', 'uint64 .* 2.5', ...
          'int64 > 2^53 .* 2.5', 'int64 > 2^53 ./ 3', 'int64 * 1000', ...
          'via double .* 2.5', 'via double ./ 3', 'int32 .* 2.5', 'int32 ./ 3', 'double .* 2.5'};
t = zeros(nb, numel(labels));
for k = 1:nb
  tic(); R = I64 .* 2.5; t(k, 1) = toc();
  tic(); R = I64 ./ 3; t(k, 2) = toc();
  tic(); R = 3 ./ I64; t(k, 3) = toc();
  tic(); R = U64 .* 2.5; t(k, 4) = toc();
  tic(); R = B64 .* 2.5; t(k, 5) = toc();
  tic(); R = B64 ./ 3; t(k, 6) = toc();
  tic(); R = I64 * 1000; t(k, 7) = toc();
  tic(); R = int64(double(I64) .* 2.5); t(k, 8) = toc();
  tic(); R = int64(double(I64) ./ 3); t(k, 9) = toc();
  tic(); R = I32 .* 2.5; t(k, 10) = toc();
  tic(); R = I32 ./ 3; t(k, 11) = toc();
  tic(); R = D .* 2.5; t(k, 12) = toc();
end
for j = 1:numel(labels)
  fprintf('%-22s %8.2f ms\n', labels{j}, 1000 * min(t(:, j)));
end
%=============================================================================
