%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [R, tieadj] = tiedrank(X, varargin)
  narginchk(1, 3);
  nargoutchk(0, 2);
  if ~(isnumeric(X) && isreal(X) && ~issparse(X))
    error('Nelson:statistics:xMustBeRealArray', '%s must be a real numeric array.', 'X');
  end
  [kendall, bidirectional] = parseOptionsLocal(varargin);
  [R, tieadj] = tiedrankArrayLocal(double(X), kendall, bidirectional, isvector(X), size(X));
end
%=============================================================================
function [kendall, bidirectional] = parseOptionsLocal(args)
  kendall = false;
  bidirectional = false;
  if length(args) >= 1
    kendall = parseLogicalScalarLocal(args{1}, 'KENDALL');
  end
  if length(args) >= 2
    bidirectional = parseLogicalScalarLocal(args{2}, 'BIDIRECTIONAL');
  end
end
%=============================================================================
function value = parseLogicalScalarLocal(value, name)
  if ~((isnumeric(value) || islogical(value)) && isreal(value) && isscalar(value) ...
      && (value == 0 || value == 1))
    error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', name);
  end
  value = logical(value);
end
%=============================================================================
function [R, tieadj] = tiedrankArrayLocal(X, kendall, bidirectional, isVectorInput, originalSize)
  if isVectorInput
    [ranks, tieadj] = tiedrankVectorLocal(X(:), kendall, bidirectional);
    R = reshape(ranks, originalSize);
    return
  end
  sz = originalSize;
  rows = sz(1);
  cols = prod(sz(2:end));
  X = reshape(X, rows, cols);
  R = NaN(rows, cols);
  if kendall
    tieadj = zeros(3, cols);
  else
    tieadj = zeros(1, cols);
  end
  for k = 1:cols
    [R(:, k), tieadj(:, k)] = tiedrankVectorLocal(X(:, k), kendall, bidirectional);
  end
  R = reshape(R, sz);
  if length(sz) > 2
    if kendall
      tieadj = reshape(tieadj, [3 sz(2:end)]);
    else
      tieadj = reshape(tieadj, [1 sz(2:end)]);
    end
  end
end
%=============================================================================
function [ranks, tieadj] = tiedrankVectorLocal(values, kendall, bidirectional)
  ranks = NaN(size(values));
  keep = ~isnan(values);
  if ~any(keep)
    tieadj = tieAdjustmentLocal([], kendall);
    return
  end
  validValues = values(keep);
  [sortedValues, order] = sort(validValues(:));
  n = length(sortedValues);
  localRanks = zeros(n, 1);
  if n == 1
    starts = 1;
  else
    starts = [1; find(diff(sortedValues) ~= 0) + 1];
  end
  stops = [starts(2:end) - 1; n];
  tieLengths = stops - starts + 1;
  for k = 1:length(starts)
    rows = starts(k):stops(k);
    if bidirectional
      baseRanks = min(rows, n - rows + 1);
      localRanks(order(rows)) = mean(baseRanks);
    else
      localRanks(order(rows)) = (starts(k) + stops(k)) / 2;
    end
  end
  ranks(keep) = localRanks;
  tieadj = tieAdjustmentLocal(tieLengths(tieLengths > 1), kendall);
end
%=============================================================================
function tieadj = tieAdjustmentLocal(tieLengths, kendall)
  if isempty(tieLengths)
    if kendall
      tieadj = zeros(3, 1);
    else
      tieadj = 0;
    end
    return
  end
  t = double(tieLengths(:));
  if kendall
    tieadj = [sum(t .* (t - 1) / 2); ...
              sum(t .* (t - 1) .* (t - 2)); ...
              sum(t .* (t - 1) .* (2 * t + 5))];
  else
    tieadj = sum(t .^ 3 - t) / 2;
  end
end
%=============================================================================
