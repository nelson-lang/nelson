%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% inputs longer than one gathered block are reduced chunk by chunk;
% integer-valued data keeps every window sum exact.
n = 1500000;
x = mod(1:n, 7);
s = x + [x(1), x(1:end - 1)] + [x(2:end), x(end)];
s(1) = x(1) + x(2);
s(end) = x(end - 1) + x(end);
counts = [2, 3 * ones(1, n - 2), 2];
asserts.isequal(movsum(x, 3), s);
asserts.isequal(movmean(x, 3), s ./ counts);
asserts.isequal(movsum(x, 3, 'Endpoints', 'discard'), s(2:end - 1));
asserts.isequal(movsum(x, 3, 'Endpoints', 'fill'), [NaN, s(2:end - 1), NaN]);
%=============================================================================
% matrix: output positions run down every column across the chunks
X = mod(reshape(1:2000000, [], 2), 7);
S = X + [X(1, :); X(1:end - 1, :)] + [X(2:end, :); X(end, :)];
S(1, :) = X(1, :) + X(2, :);
S(end, :) = X(end - 1, :) + X(end, :);
asserts.isequal(movsum(X, 3), S);
asserts.isequal(movsum(X, 3, 2), repmat(X(:, 1) + X(:, 2), 1, 2));
%=============================================================================
% a different window width changes the chunk size
n = 1000000;
x = mod(1:n, 11);
m = x;
for o = [-2 -1 1 2]
  idx = min(max((1:n) + o, 1), n);
  m = max(m, x(idx));
end
asserts.isequal(movmax(x, 5), m);
%=============================================================================
