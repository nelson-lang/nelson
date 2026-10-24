%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [rho, pval] = corr(X, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 2);
  [Y, opts] = parseInputsLocal(X, convertStringToCharArgs(varargin));
  [X, Y] = validateInputsLocal(X, Y);
  if strcmp(opts.Rows, 'complete')
    [X, Y] = completeRowsLocal(X, Y);
  end
  if strcmp(opts.Type, 'spearman')
    X = rankColumnsLocal(X);
    Y = rankColumnsLocal(Y);
  end
  if strcmp(opts.Type, 'kendall')
    rho = pairwiseKendallLocal(X, Y, opts.Rows);
    n = pairwiseCountsLocal(X, Y, opts.Rows);
    pval = pvalueKendallLocal(rho, n, opts.Tail);
  else
    [rho, n] = pairwisePearsonLocal(X, Y, opts.Rows, opts.Weights);
    pval = pvaluePearsonLocal(rho, n, opts.Tail, ~isempty(opts.Weights));
  end
end
%=============================================================================
function [Y, opts] = parseInputsLocal(X, args)
  opts = struct('Type', 'pearson', 'Rows', 'all', 'Tail', 'both', 'Weights', []);
  Y = [];
  if ~isempty(args) && ~isTextScalarLocal(args{1})
    Y = args{1};
    args = args(2:end);
  end
  if isempty(Y)
    Y = X;
  end
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{k}));
    switch name
      case 'type'
        opts.Type = parseChoiceLocal(args{k + 1}, {'pearson', 'kendall', 'spearman'}, 'TYPE');
      case 'rows'
        opts.Rows = parseChoiceLocal(args{k + 1}, {'all', 'complete', 'pairwise'}, 'ROWS');
      case 'tail'
        opts.Tail = parseChoiceLocal(args{k + 1}, {'both', 'right', 'left'}, 'TAIL');
      case 'weights'
        opts.Weights = parseWeightsLocal(args{k + 1}, size(X, 1));
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
end
%=============================================================================
function [X, Y] = validateInputsLocal(X, Y)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ismatrix(X))
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if ~(isnumeric(Y) && isreal(Y) && ~issparse(Y) && ismatrix(Y))
    error('Nelson:statistics:yRealNumericMatrix', '%s must be a real numeric matrix.', 'Y');
  end
  if size(X, 1) ~= size(Y, 1)
    error('Nelson:statistics:xYSameNumberRows', '%s must have the same number of rows.', 'X and Y');
  end
  X = double(X);
  Y = double(Y);
end
%=============================================================================
function weights = parseWeightsLocal(value, n)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && length(value) == n)
    error('Nelson:statistics:weightsNumericVectorOneValue', '%s must be a numeric vector with one value per observation.', 'Weights');
  end
  weights = double(value(:));
  if any(isnan(weights)) || any(weights < 0)
    error('Nelson:statistics:weightsContainNonnegativeFiniteValues', '%s must contain nonnegative finite values.', 'Weights');
  end
  if all(weights == 0)
    error('Nelson:validators:mustBePositive', '%s must be positive.', 'At least one weight');
  end
end
%=============================================================================
function [X, Y] = completeRowsLocal(X, Y)
  keep = all(~isnan(X), 2) & all(~isnan(Y), 2);
  X = X(keep, :);
  Y = Y(keep, :);
end
%=============================================================================
function R = rankColumnsLocal(X)
  R = NaN(size(X));
  for k = 1:size(X, 2)
    R(:, k) = tiedrank(X(:, k));
  end
end
%=============================================================================
function [rho, n] = pairwisePearsonLocal(X, Y, rowsMode, weights)
  if isempty(weights) && (~strcmp(rowsMode, 'pairwise') || (~any(isnan(X(:))) && ~any(isnan(Y(:)))))
    [rho, n] = pearsonMatrixLocal(X, Y);
    return
  end
  if ~isempty(weights) && (~strcmp(rowsMode, 'pairwise') || (~any(isnan(X(:))) && ~any(isnan(Y(:)))))
    [rho, n] = weightedPearsonMatrixLocal(X, Y, weights);
    return
  end
  nx = size(X, 2);
  ny = size(Y, 2);
  rho = NaN(nx, ny);
  n = zeros(nx, ny);
  for i = 1:nx
    for j = 1:ny
      keep = validRowsLocal(X(:, i), Y(:, j), rowsMode);
      xi = X(keep, i);
      yj = Y(keep, j);
      if isempty(weights)
        [rho(i, j), n(i, j)] = pearsonVectorLocal(xi, yj);
      else
        [rho(i, j), n(i, j)] = weightedPearsonVectorLocal(xi, yj, weights(keep));
      end
    end
  end
end
%=============================================================================
function [rho, n] = pearsonMatrixLocal(X, Y)
  n = size(X, 1) * ones(size(X, 2), size(Y, 2));
  if size(X, 1) < 2
    rho = NaN(size(X, 2), size(Y, 2));
    return
  end
  X = X - mean(X, 1);
  Y = Y - mean(Y, 1);
  sx = sqrt(sum(X .* X, 1));
  sy = sqrt(sum(Y .* Y, 1));
  rho = (X' * Y) ./ (sx' * sy);
  rho((sx' * sy) == 0) = NaN;
end
%=============================================================================
function [rho, n] = weightedPearsonMatrixLocal(X, Y, weights)
  n = size(X, 1) * ones(size(X, 2), size(Y, 2));
  if size(X, 1) < 2 || any(isnan(X(:))) || any(isnan(Y(:)))
    rho = NaN(size(X, 2), size(Y, 2));
    return
  end
  w = weights(:);
  sw = sum(w);
  mx = sum(X .* w, 1) / sw;
  my = sum(Y .* w, 1) / sw;
  X = X - mx;
  Y = Y - my;
  sx = sqrt(sum((X .* X) .* w, 1));
  sy = sqrt(sum((Y .* Y) .* w, 1));
  rho = (X' * (Y .* w)) ./ (sx' * sy);
  rho((sx' * sy) == 0) = NaN;
end
%=============================================================================
function keep = validRowsLocal(x, y, rowsMode)
  switch rowsMode
    case 'all'
      keep = true(size(x));
    otherwise
      keep = ~isnan(x) & ~isnan(y);
  end
end
%=============================================================================
function [r, n] = pearsonVectorLocal(x, y)
  n = length(x);
  if n < 2 || any(isnan(x)) || any(isnan(y))
    r = NaN;
    return
  end
  x = x - mean(x);
  y = y - mean(y);
  den = sqrt(sum(x .* x) * sum(y .* y));
  if den == 0
    r = NaN;
  else
    r = sum(x .* y) / den;
  end
end
%=============================================================================
function [r, n] = weightedPearsonVectorLocal(x, y, w)
  keep = ~isnan(x) & ~isnan(y) & (w > 0);
  x = x(keep);
  y = y(keep);
  w = w(keep);
  n = length(x);
  if n < 2
    r = NaN;
    return
  end
  sw = sum(w);
  mx = sum(w .* x) / sw;
  my = sum(w .* y) / sw;
  x = x - mx;
  y = y - my;
  den = sqrt(sum(w .* x .* x) * sum(w .* y .* y));
  if den == 0
    r = NaN;
  else
    r = sum(w .* x .* y) / den;
  end
end
%=============================================================================
function rho = pairwiseKendallLocal(X, Y, rowsMode)
  nx = size(X, 2);
  ny = size(Y, 2);
  rho = NaN(nx, ny);
  for i = 1:nx
    for j = 1:ny
      keep = validRowsLocal(X(:, i), Y(:, j), rowsMode);
      rho(i, j) = kendallVectorLocal(X(keep, i), Y(keep, j));
    end
  end
end
%=============================================================================
function r = kendallVectorLocal(x, y)
  if length(x) < 2 || any(isnan(x)) || any(isnan(y))
    r = NaN;
    return
  end
  concordant = 0;
  discordant = 0;
  tx = 0;
  ty = 0;
  n = length(x);
  for a = 1:n - 1
    dx = x(a) - x(a + 1:end);
    dy = y(a) - y(a + 1:end);
    sx = sign(dx);
    sy = sign(dy);
    concordant = concordant + sum(sx .* sy > 0);
    discordant = discordant + sum(sx .* sy < 0);
    tx = tx + sum(sx == 0 & sy ~= 0);
    ty = ty + sum(sy == 0 & sx ~= 0);
  end
  den = sqrt((concordant + discordant + tx) * (concordant + discordant + ty));
  if den == 0
    r = NaN;
  else
    r = (concordant - discordant) / den;
  end
end
%=============================================================================
function n = pairwiseCountsLocal(X, Y, rowsMode)
  nx = size(X, 2);
  ny = size(Y, 2);
  n = zeros(nx, ny);
  for i = 1:nx
    for j = 1:ny
      n(i, j) = sum(validRowsLocal(X(:, i), Y(:, j), rowsMode));
    end
  end
end
%=============================================================================
function p = pvaluePearsonLocal(r, n, tail, hasWeights)
  p = NaN(size(r));
  if hasWeights
    return
  end
  valid = ~isnan(r) & n > 2 & abs(r) < 1;
  t = r(valid) .* sqrt((n(valid) - 2) ./ max(1 - r(valid) .^ 2, 0));
  p(valid) = tailProbabilityFromTLocal(t, n(valid) - 2, tail);
  perfect = ~isnan(r) & n > 2 & abs(r) == 1;
  if any(perfect(:))
    p(perfect) = perfectCorrelationPValueLocal(r(perfect), tail);
  end
end
%=============================================================================
function p = pvalueKendallLocal(r, n, tail)
  p = NaN(size(r));
  valid = ~isnan(r) & n > 2;
  sigma = sqrt(2 * (2 * n(valid) + 5) ./ (9 * n(valid) .* (n(valid) - 1)));
  z = r(valid) ./ sigma;
  p(valid) = tailProbabilityFromNormalLocal(z, tail);
end
%=============================================================================
function p = tailProbabilityFromTLocal(t, v, tail)
  switch tail
    case 'right'
      p = 1 - tcdf(t, v);
    case 'left'
      p = tcdf(t, v);
    otherwise
      p = 2 * min(tcdf(t, v), 1 - tcdf(t, v));
  end
end
%=============================================================================
function p = perfectCorrelationPValueLocal(r, tail)
  p = zeros(size(r));
  switch tail
    case 'right'
      p(r < 0) = 1;
    case 'left'
      p(r > 0) = 1;
  end
end
%=============================================================================
function p = tailProbabilityFromNormalLocal(z, tail)
  switch tail
    case 'right'
      p = 1 - normcdf(z, 0, 1);
    case 'left'
      p = normcdf(z, 0, 1);
    otherwise
      p = 2 * min(normcdf(z, 0, 1), 1 - normcdf(z, 0, 1));
  end
end
%=============================================================================
function value = parseChoiceLocal(value, choices, name)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
  text = lower(char(value));
  matches = choices(startsWith(choices, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
  end
  value = matches{1};
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
