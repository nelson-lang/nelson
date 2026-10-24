%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [idx, scores] = fsrftest(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 2);
  [X, Y, opts] = parseInputsLocal(X, Y, convertStringToCharArgs(varargin));
  scores = zeros(1, size(X, 2));
  for j = 1:size(X, 2)
    scores(j) = predictorScoreLocal(X(:, j), Y, opts.CategoricalPredictors(j), opts);
  end
  [~, idx] = sort(scores, 'descend');
end
%=============================================================================
function [X, Y, opts] = parseInputsLocal(X, Y, args)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if ~(isnumeric(Y) && isreal(Y) && isvector(Y))
    error('Nelson:statistics:yRealNumericVector', '%s must be a real numeric vector.', 'Y');
  end
  if size(X, 1) ~= length(Y)
    error('Nelson:statistics:xYSameNumberRows', '%s must have the same number of rows.', 'X and Y');
  end
  X = double(X);
  Y = double(Y(:));
  if isempty(X) || size(X, 2) == 0
    error('Nelson:statistics:xContainLeastOnePredictor', 'X must contain at least one predictor.');
  end
  if any(isinf(X(:))) || any(isinf(Y(:)))
    error('Nelson:statistics:xYContainInfValues', '%s must not contain Inf values.', 'X and Y');
  end
  opts = defaultOptionsLocal(size(X, 2), size(X, 1));
  opts = parseNameValuesLocal(opts, args);
  opts.CategoricalPredictors = parseCategoricalLocal(opts.CategoricalPredictors, size(X, 2));
  opts.NumBins = parseNumBinsLocal(opts.NumBins);
  opts.UseMissing = parseLogicalScalarLocal(opts.UseMissing, 'UseMissing');
  opts.Weights = parseWeightsLocal(opts.Weights, size(X, 1));
end
%=============================================================================
function opts = defaultOptionsLocal(nvars, nobs)
  opts = struct('CategoricalPredictors', false(1, nvars), ...
    'NumBins', 10, 'UseMissing', false, 'Weights', ones(nobs, 1));
end
%=============================================================================
function opts = parseNameValuesLocal(opts, args)
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{k}));
    switch name
      case 'categoricalpredictors'
        opts.CategoricalPredictors = args{k + 1};
      case 'numbins'
        opts.NumBins = args{k + 1};
      case 'usemissing'
        opts.UseMissing = args{k + 1};
      case 'weights'
        opts.Weights = args{k + 1};
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
  end
end
%=============================================================================
function categorical = parseCategoricalLocal(value, nvars)
  categorical = false(1, nvars);
  if isempty(value)
    return
  end
  if isTextScalarLocal(value)
    switch lower(char(value))
      case 'all'
        categorical(:) = true;
      case 'none'
        categorical(:) = false;
      otherwise
        error('Nelson:statistics:categoricalPredictorsInvalidValue', '%s has an invalid value.', 'CategoricalPredictors');
    end
  elseif islogical(value) && isvector(value)
    if length(value) ~= nvars
      error('Nelson:statistics:categoricalPredictorsMatchNumberPredictors', '%s must match the number of predictors.', 'CategoricalPredictors');
    end
    categorical = value(:)' ~= 0;
  elseif isnumeric(value) && isreal(value) && isvector(value)
    idx = double(value(:))';
    if any(~isfinite(idx)) || any(floor(idx) ~= idx) || any(idx < 1) || any(idx > nvars)
      error('Nelson:statistics:categoricalPredictorsContainsInvalidPredictorIndices', '%s contains invalid predictor indices.', 'CategoricalPredictors');
    end
    categorical(idx) = true;
  else
    error('Nelson:statistics:categoricalPredictorsInvalidValue', '%s has an invalid value.', 'CategoricalPredictors');
  end
end
%=============================================================================
function numBins = parseNumBinsLocal(numBins)
  if ~(isnumeric(numBins) && isreal(numBins) && isscalar(numBins) ...
      && isfinite(numBins) && floor(numBins) == numBins && numBins >= 2)
    error('Nelson:statistics:numBinsIntegerScalarGreaterOne', 'NumBins must be an integer scalar greater than one.');
  end
  numBins = double(numBins);
end
%=============================================================================
function weights = parseWeightsLocal(weights, nobs)
  if ~(isnumeric(weights) && isreal(weights) && isvector(weights) && length(weights) == nobs)
    error('Nelson:statistics:weightsRealNumericVectorOne', 'Weights must be a real numeric vector with one value per observation.');
  end
  weights = double(weights(:));
  if any(~isfinite(weights)) || any(weights < 0) || sum(weights) <= 0
    error('Nelson:statistics:weightsContainNonnegativeFiniteValues:valueContainNonnegativeFiniteValuesPositive', '%s must contain nonnegative finite values with positive sum.', 'Weights');
  end
end
%=============================================================================
function score = predictorScoreLocal(x, y, categorical, opts)
  valid = validRowsLocal(x, y, opts.Weights, opts.UseMissing);
  x = x(valid);
  y = y(valid);
  weights = opts.Weights(valid);
  if length(y) < 3 || weightedVarianceLocal(y, weights) <= 0
    score = 0;
  elseif categorical
    score = categoricalScoreLocal(x, y, weights, opts.UseMissing);
  else
    score = continuousScoreLocal(x, y, weights, opts);
  end
  if ~isfinite(score) || score < 0
    score = 0;
  end
end
%=============================================================================
function valid = validRowsLocal(x, y, weights, useMissing)
  valid = weights > 0 & ~isnan(y);
  if ~useMissing
    valid = valid & ~isnan(x);
  end
end
%=============================================================================
function score = continuousScoreLocal(x, y, weights, opts)
  if opts.NumBins > 2 && length(unique(x(~isnan(x)))) > opts.NumBins
    groups = binContinuousLocal(x, opts.NumBins, opts.UseMissing);
    score = anovaScoreLocal(groups, y, weights);
  else
    score = linearScoreLocal(x, y, weights);
  end
end
%=============================================================================
function score = linearScoreLocal(x, y, weights)
  keep = ~isnan(x);
  x = x(keep);
  y = y(keep);
  weights = weights(keep);
  if length(y) < 3 || weightedVarianceLocal(x, weights) <= 0
    score = 0;
    return
  end
  mx = weightedMeanLocal(x, weights);
  my = weightedMeanLocal(y, weights);
  sx = sum(weights .* (x - mx) .^ 2);
  sy = sum(weights .* (y - my) .^ 2);
  sxy = sum(weights .* (x - mx) .* (y - my));
  r2 = min(max((sxy ^ 2) / max(sx * sy, realmin), 0), 1);
  if r2 >= 1
    score = realmax;
  else
    score = (r2 / max(1 - r2, realmin)) * max(length(y) - 2, 1);
  end
end
%=============================================================================
function score = categoricalScoreLocal(x, y, weights, useMissing)
  if useMissing && any(isnan(x))
    missingCode = max(x(~isnan(x))) + 1;
    if isempty(missingCode) || ~isfinite(missingCode)
      missingCode = 1;
    end
    x(isnan(x)) = missingCode;
  end
  score = anovaScoreLocal(x, y, weights);
end
%=============================================================================
function score = anovaScoreLocal(groups, y, weights)
  [labels, ~, groupIndex] = unique(groups(:));
  validLabels = ~isnan(labels);
  labels = labels(validLabels);
  if length(labels) < 2
    score = 0;
    return
  end
  grandMean = weightedMeanLocal(y, weights);
  between = 0;
  within = 0;
  nonempty = 0;
  validOriginal = find(validLabels);
  for k = 1:length(labels)
    keep = groupIndex == validOriginal(k);
    if ~any(keep)
      continue
    end
    wk = weights(keep);
    yk = y(keep);
    groupMean = weightedMeanLocal(yk, wk);
    between = between + sum(wk) * (groupMean - grandMean) ^ 2;
    within = within + sum(wk .* (yk - groupMean) .^ 2);
    nonempty = nonempty + 1;
  end
  dfBetween = max(nonempty - 1, 1);
  dfWithin = max(length(y) - nonempty, 1);
  if within <= 0
    if between > 0
      score = realmax;
    else
      score = 0;
    end
  else
    score = (between / dfBetween) / max(within / dfWithin, realmin);
  end
end
%=============================================================================
function groups = binContinuousLocal(x, numBins, useMissing)
  groups = NaN(size(x));
  keep = ~isnan(x);
  values = x(keep);
  [~, order] = sort(values);
  binNumbers = ceil((1:length(values)) * numBins / max(length(values), 1));
  binNumbers(binNumbers < 1) = 1;
  binNumbers(binNumbers > numBins) = numBins;
  temp = zeros(size(values));
  temp(order) = binNumbers;
  groups(keep) = temp;
  if useMissing && any(~keep)
    groups(~keep) = numBins + 1;
  end
end
%=============================================================================
function value = weightedMeanLocal(x, weights)
  value = sum(weights .* x) / sum(weights);
end
%=============================================================================
function value = weightedVarianceLocal(x, weights)
  keep = ~isnan(x);
  x = x(keep);
  weights = weights(keep);
  if isempty(x) || sum(weights) <= 0
    value = 0;
    return
  end
  mu = weightedMeanLocal(x, weights);
  value = sum(weights .* (x - mu) .^ 2) / sum(weights);
end
%=============================================================================
function tf = parseLogicalScalarLocal(value, name)
  if islogical(value) && isscalar(value)
    tf = value ~= 0;
  elseif isnumeric(value) && isscalar(value) && (value == 0 || value == 1)
    tf = value ~= 0;
  else
    error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', name);
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
