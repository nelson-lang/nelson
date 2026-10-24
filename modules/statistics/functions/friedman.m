%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [p, tbl, stats] = friedman(X, reps, displayopt)
  narginchk(1, 3);
  nargoutchk(0, 3);
  hasDisplay = nargin >= 3;
  if nargin < 2
    reps = 1;
  elseif nargin == 2 && isDisplayOptionLocal(reps)
    displayopt = reps;
    reps = 1;
    hasDisplay = true;
  end
  if ~hasDisplay
    displayopt = 'on';
  end
  displayopt = parseDisplayLocal(displayopt);
  X = validateDataLocal(X);
  reps = validateRepsLocal(reps, size(X, 1));
  result = friedmanTableLocal(X, reps);
  p = result.p;
  tbl = result.tbl;
  stats = result.stats;
  if strcmp(displayopt, 'on')
    disp(tbl);
  end
end
%=============================================================================
function X = validateDataLocal(X)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  X = double(X);
  if isempty(X) || any(isnan(X(:)))
    error('Nelson:statistics:xContainNonNaNObservations', 'X must contain non-NaN observations.');
  end
  if size(X, 1) < 2 || size(X, 2) < 2
    error('Nelson:statistics:xLeastTwoRowsTwo', '%s must have at least two rows and two columns.', 'X');
  end
end
%=============================================================================
function reps = validateRepsLocal(reps, nrows)
  if ~(isnumeric(reps) && isreal(reps) && isscalar(reps) && isfinite(reps) ...
      && reps >= 1 && floor(reps) == reps)
    error('Nelson:statistics:REPSPositiveIntegerScalar', '%s must be a positive integer scalar.', 'REPS');
  end
  reps = double(reps);
  if rem(nrows, reps) ~= 0
    error('Nelson:statistics:numberRowsXMultipleREPS', '%s must be a multiple of REPS.', 'The number of rows in X');
  end
  if nrows / reps < 2
    error('Nelson:statistics:xContainLeastTwoBlock', 'X must contain at least two block levels.');
  end
end
%=============================================================================
function result = friedmanTableLocal(X, reps)
  nBlocks = size(X, 1) / reps;
  nTreatments = size(X, 2);
  [rankSums, blockTieSum] = treatmentRankSumsLocal(X, reps, nBlocks, nTreatments);
  correction = tieCorrectionLocal(blockTieSum, nBlocks, nTreatments);
  chiSquare = friedmanStatisticLocal(rankSums, nBlocks, reps, nTreatments, correction);
  dfTreatments = nTreatments - 1;
  p = chi2cdf(chiSquare, dfTreatments, 'upper');
  ssTreatments = sum(rankSums .^ 2) / (nBlocks * reps) ...
    - nBlocks * reps * nTreatments * (nTreatments + 1) ^ 2 / 4;
  ssTotal = nBlocks * reps * nTreatments * (nTreatments ^ 2 - 1) / 12;
  ssError = ssTotal - ssTreatments;
  result.p = p;
  result.tbl = cellTableLocal(ssTreatments, ssError, ssTotal, dfTreatments, nBlocks, reps, chiSquare, p);
  result.stats = struct('source', 'friedman', 'n', nBlocks, 'reps', reps, ...
    'meanranks', rankSums / (nBlocks * reps), 'sumranks', rankSums, 'df', dfTreatments);
end
%=============================================================================
function [rankSums, blockTieSum] = treatmentRankSumsLocal(X, reps, nBlocks, nTreatments)
  rankSums = zeros(1, nTreatments);
  blockTieSum = 0;
  for b = 1:nBlocks
    rows = (b - 1) * reps + 1:b * reps;
    block = X(rows, :);
    for r = 1:reps
      [ranks, tieSum] = tiedRanksRowLocal(block(r, :));
      rankSums = rankSums + ranks;
      blockTieSum = blockTieSum + tieSum;
    end
  end
end
%=============================================================================
function [ranks, tieSum] = tiedRanksRowLocal(values)
  [sortedValues, order] = sort(values(:));
  k = length(sortedValues);
  ranks = zeros(1, k);
  tieSum = 0;
  starts = [1; find(diff(sortedValues) ~= 0) + 1];
  stops = [starts(2:end) - 1; k];
  for j = 1:length(starts)
    rows = starts(j):stops(j);
    ranks(order(rows)) = (starts(j) + stops(j)) / 2;
    tieLength = stops(j) - starts(j) + 1;
    tieSum = tieSum + tieLength ^ 3 - tieLength;
  end
end
%=============================================================================
function correction = tieCorrectionLocal(blockTieSum, nBlocks, nTreatments)
  denominator = nBlocks * (nTreatments ^ 3 - nTreatments);
  correction = 1 - blockTieSum / denominator;
end
%=============================================================================
function value = friedmanStatisticLocal(rankSums, nBlocks, reps, nTreatments, correction)
  value = 12 / (nBlocks * reps * nTreatments * (nTreatments + 1)) * sum(rankSums .^ 2) ...
    - 3 * nBlocks * reps * (nTreatments + 1);
  if correction == 0
    value = 0;
  else
    value = value / correction;
  end
end
%=============================================================================
function tbl = cellTableLocal(ssTreatments, ssError, ssTotal, dfTreatments, nBlocks, reps, chiSquare, p)
  tbl = {'Source', 'SS', 'df', 'Chi-sq', 'Prob>Chi-sq'; ...
         'Columns', ssTreatments, dfTreatments, chiSquare, p; ...
         'Error', ssError, (nBlocks * reps - 1) * dfTreatments, [], []; ...
         'Total', ssTotal, nBlocks * reps * dfTreatments, [], []};
end
%=============================================================================
function opt = parseDisplayLocal(value)
  if ~isDisplayOptionLocal(value)
    error('Nelson:statistics:displayOptionOff', 'Display option must be ''on'' or ''off''.');
  end
  opt = lower(char(value));
end
%=============================================================================
function tf = isDisplayOptionLocal(value)
  tf = false;
  if isTextScalarLocal(value)
    text = lower(char(value));
    tf = strcmp(text, 'on') || strcmp(text, 'off');
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
