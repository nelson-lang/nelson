%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [Fm, fm] = fillmissingKnn(X, M, loc, k, distance)
  % Each missing entry takes the mean of the values of the k nearest rows
  % holding one. The distance is measured on the nonmissing coordinates of
  % the row; a row missing one of them is no neighbor. Ties keep the row
  % order. The rows with missing entries are processed by blocks, the
  % candidate rows running along the columns of the distance matrices.
  [n, p] = size(X);
  count = length(loc.miss);
  Fm = NaN(count, 1);
  fm = false(count, 1);
  U = double(X);
  V = U;
  V(M) = NaN;
  scale = [];
  if ischar(distance) && strcmp(distance, 'seuclidean')
    scale = std(V, 0, 1, 'omitnan');
  end
  % Coordinates for the distances: missing ones are zero, masked by M.
  V(M) = 0;
  [rows, order] = sort(loc.row);
  targets = rows([true; diff(rows) ~= 0]);
  blockPosition = zeros(n, 1);
  % Blocks of about 1 MB distance matrices: larger allocations are slower and gain nothing.
  blockSize = max(1, floor(131072 / n));
  for first = 1:blockSize:length(targets)
    T = targets(first:min(end, first + blockSize - 1));
    t = length(T);
    d = knnDistances(X, V, M, T, distance, scale);
    d(T + (0:t - 1)' * n) = NaN;
    inBlock = rows >= T(1) & rows <= T(end);
    entries = order(inBlock);
    blockPosition(T) = 1:t;
    % One column per missing entry: the distances of the rows holding its value.
    dE = d(:, blockPosition(rows(inBlock)));
    cols = loc.col(entries);
    dE(M(:, cols)) = NaN;
    [total, found] = knnSelect(dE, U, cols, k);
    Fm(entries) = total ./ found;
    fm(entries) = found > 0;
  end
end
%=============================================================================
function [total, found] = knnSelect(dE, U, cols, k)
  % Sum and count of the values of the k smallest distances of each column
  % of dE (NaN: no neighbor), the first row winning ties. Small k: k
  % passes of min, which also returns the first index; otherwise a stable sort.
  [n, E] = size(dE);
  offsets = (cols(:)' - 1) * n;
  if k < log2(n)
    total = zeros(1, E);
    found = zeros(1, E);
    columns = (0:E - 1) * n;
    for q = 1:k
      [m, r] = min(dE, [], 1);
      valid = ~isnan(m);
      if ~any(valid)
        break
      end
      % reshape: indexing a single-column U would return a column.
      total(valid) = total(valid) + reshape(U(r(valid) + offsets(valid)), 1, []);
      found = found + valid;
      dE(r(valid) + columns(valid)) = NaN;
    end
  else
    [dE, r] = sort(dE, 1);
    kk = min(k, n);
    valid = ~isnan(dE(1:kk, :));
    values = reshape(U(r(1:kk, :) + offsets), kk, E);
    values(~valid) = 0;
    total = sum(values, 1);
    found = sum(valid, 1);
  end
end
%=============================================================================
function d = knnDistances(X, V, M, T, distance, scale)
  % Squared distances (built-in metrics) from every row (rows of d) to the
  % rows T (columns of d), NaN for the rows that cannot be compared.
  n = size(X, 1);
  t = length(T);
  if isa(distance, 'function_handle')
    d = NaN(n, t);
    for a = 1:t
      i = T(a);
      for r = [1:i - 1, i + 1:n]
        d(r, a) = double(distance([X(i, :); X(r, :)], [M(i, :); M(r, :)]));
      end
    end
    return
  end
  % Squares are accumulated coordinate by coordinate over the coordinates
  % of each target row; a row missing one of them gets NaN. The squared
  % distance is kept: a square root could merge distinct sums into false
  % ties and change the neighbors.
  VT = V(T, :)';
  P = ~M(T, :)';
  d = zeros(n, t);
  for j = 1:size(V, 2)
    delta = V(:, j) - VT(j, :);
    if ~isempty(scale)
      % 'seuclidean': each difference is divided by the standard deviation of its coordinate.
      delta = delta / scale(j);
    end
    delta = delta .* delta;
    if ~all(P(j, :))
      delta(:, ~P(j, :)) = 0;
    end
    d = d + delta;
  end
  d((double(M) * double(P)) > 0) = NaN;
end
%=============================================================================
