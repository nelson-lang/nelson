%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [idx, weights] = relieff(X, y, k, varargin)
  narginchk(3, Inf);
  nargoutchk(0, 2);
  [X, y, k, opts] = parseInputsLocal(X, y, k, convertStringToCharArgs(varargin));
  featureDiff = featureDifferencesLocal(X, opts.CategoricalX);
  if strcmp(opts.Method, 'classification')
    weights = classificationWeightsLocal(featureDiff, y, k, opts);
  else
    weights = regressionWeightsLocal(featureDiff, y, k, opts);
  end
  [~, idx] = sort(weights, 'descend');
end
%=============================================================================
function [X, y, k, opts] = parseInputsLocal(X, y, k, args)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  X = double(X);
  if isempty(X) || any(~isfinite(X(:)))
    error('Nelson:statistics:xMustBeFinite', '%s must contain finite values.', 'X');
  end
  if ~(isvector(y) && length(y) == size(X, 1))
    error('Nelson:statistics:yVectorOneElementObservation', 'y must be a vector with one element per observation.');
  end
  if ~(isnumeric(k) && isreal(k) && isscalar(k) && isfinite(k) ...
      && floor(k) == k && k > 0)
    error('Nelson:statistics:kPositiveIntegerScalar', '%s must be a positive integer scalar.', 'k');
  end
  k = double(k);
  opts = defaultOptionsLocal(X, y);
  opts = parseNameValuesLocal(opts, args, X, y);
  opts.Updates = parseUpdatesLocal(opts.Updates, size(X, 1));
  opts.CategoricalX = parseCategoricalXLocal(opts.CategoricalX, size(X, 2));
  opts.Prior = parsePriorLocal(opts.Prior, y, opts.Method);
  opts.Sigma = parseSigmaLocal(opts.Sigma);
end
%=============================================================================
function opts = defaultOptionsLocal(X, y)
  method = 'regression';
  if ~(isnumeric(y) || islogical(y))
    method = 'classification';
  end
  opts = struct('Method', method, 'Prior', 'empirical', ...
    'Updates', size(X, 1), 'CategoricalX', [], 'Sigma', []);
end
%=============================================================================
function opts = parseNameValuesLocal(opts, args, X, y)
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for i = 1:2:length(args)
    if ~isTextScalarLocal(args{i})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{i}));
    switch name
      case 'method'
        opts.Method = parseMethodLocal(args{i + 1});
      case 'prior'
        opts.Prior = args{i + 1};
      case 'updates'
        opts.Updates = args{i + 1};
      case 'categoricalx'
        opts.CategoricalX = args{i + 1};
      case 'sigma'
        opts.Sigma = args{i + 1};
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
  end
  if strcmp(opts.Method, 'regression') && ~(isnumeric(y) || islogical(y))
    error('Nelson:statistics:regressionMethodRequiresNumericResponse', 'Regression method requires a numeric response.');
  end
end
%=============================================================================
function method = parseMethodLocal(value)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Method');
  end
  value = lower(char(value));
  choices = {'classification', 'regression'};
  matches = choices(strncmp(choices, value, length(value)));
  if isempty(matches)
    error('Nelson:statistics:invalidMethodValue', 'Invalid %s value.', 'Method');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousValue', 'Ambiguous %s value.', 'Method');
  end
  method = matches{1};
end
%=============================================================================
function updates = parseUpdatesLocal(updates, nobs)
  if isempty(updates)
    updates = nobs;
    return
  end
  if isnumeric(updates) && isreal(updates) && isscalar(updates) ...
      && isfinite(updates) && floor(updates) == updates && updates > 0
    updates = 1:min(double(updates), nobs);
  elseif isnumeric(updates) && isreal(updates) && isvector(updates)
    updates = double(updates(:))';
    if any(~isfinite(updates)) || any(floor(updates) ~= updates) ...
        || any(updates < 1) || any(updates > nobs)
      error('Nelson:statistics:updatesContainValidObservationIndices', 'Updates must contain valid observation indices.');
    end
  else
    error('Nelson:statistics:updatesPositiveIntegerScalarVector', 'Updates must be a positive integer scalar or vector of indices.');
  end
end
%=============================================================================
function categorical = parseCategoricalXLocal(value, nvars)
  categorical = false(1, nvars);
  if isempty(value)
    return
  end
  if isTextScalarLocal(value)
    switch lower(char(value))
      case 'on'
        categorical(:) = true;
      case 'off'
        categorical(:) = false;
      otherwise
        error('Nelson:statistics:categoricalPredictorsInvalidValue', '%s has an invalid value.', 'CategoricalX');
    end
  elseif islogical(value) && isvector(value)
    if length(value) ~= nvars
      error('Nelson:statistics:categoricalPredictorsMatchNumberPredictors', '%s must match the number of predictors.', 'CategoricalX');
    end
    categorical = value(:)' ~= 0;
  elseif isnumeric(value) && isreal(value) && isvector(value)
    indices = double(value(:))';
    if any(~isfinite(indices)) || any(floor(indices) ~= indices) ...
        || any(indices < 1) || any(indices > nvars)
      error('Nelson:statistics:categoricalPredictorsContainsInvalidPredictorIndices', '%s contains invalid predictor indices.', 'CategoricalX');
    end
    categorical(indices) = true;
  else
    error('Nelson:statistics:categoricalPredictorsInvalidValue', '%s has an invalid value.', 'CategoricalX');
  end
end
%=============================================================================
function prior = parsePriorLocal(prior, y, method)
  if strcmp(method, 'regression')
    prior = [];
    return
  end
  [~, classes] = classIndicesLocal(y);
  nclasses = length(classes);
  if isTextScalarLocal(prior)
    value = lower(char(prior));
    if strcmp(value, 'empirical')
      counts = zeros(1, nclasses);
      labels = labelsLocal(y);
      for c = 1:nclasses
        counts(c) = sum(labels == classes(c));
      end
      prior = counts / sum(counts);
    elseif strcmp(value, 'uniform')
      prior = ones(1, nclasses) / nclasses;
    else
      error('Nelson:statistics:priorInvalidValue', '%s has an invalid value.', 'Prior');
    end
  elseif isnumeric(prior) && isreal(prior) && isvector(prior) && length(prior) == nclasses
    prior = double(prior(:))';
    if any(~isfinite(prior)) || any(prior < 0) || sum(prior) <= 0
      error('Nelson:statistics:priorContainNonnegativeFiniteValues', '%s must contain nonnegative finite values with positive sum.', 'Prior');
    end
    prior = prior / sum(prior);
  else
    error('Nelson:statistics:priorInvalidValue', '%s has an invalid value.', 'Prior');
  end
end
%=============================================================================
function sigma = parseSigmaLocal(sigma)
  if isempty(sigma)
    return
  end
  if ~(isnumeric(sigma) && isreal(sigma) && isscalar(sigma) && isfinite(sigma) && sigma > 0)
    error('Nelson:statistics:sigmaPositiveScalar', '%s must be a positive scalar.', 'Sigma');
  end
  sigma = double(sigma);
end
%=============================================================================
function featureDiff = featureDifferencesLocal(X, categorical)
  nobs = size(X, 1);
  nvars = size(X, 2);
  featureDiff = zeros(nobs, nobs, nvars);
  ranges = max(X, [], 1) - min(X, [], 1);
  ranges(ranges == 0) = 1;
  for j = 1:nvars
    values = X(:, j);
    if categorical(j)
      featureDiff(:, :, j) = double(values ~= values');
    else
      featureDiff(:, :, j) = abs(values - values') / ranges(j);
    end
  end
end
%=============================================================================
function weights = classificationWeightsLocal(featureDiff, y, k, opts)
  labels = labelsLocal(y);
  [classLabels, classes] = classIndicesLocal(y);
  nobs = length(labels);
  nvars = size(featureDiff, 3);
  weights = zeros(1, nvars);
  distances = predictorDistancesLocal(featureDiff);
  updates = opts.Updates;
  for u = 1:length(updates)
    i = updates(u);
    own = labels(i);
    hit = nearestLocal(distances(i, :), find(labels == own & (1:nobs)' ~= i), k);
    weights = weights - meanDiffLocal(featureDiff, i, hit) / length(updates);
    ownClass = find(classes == own);
    missScale = 1 - opts.Prior(ownClass);
    for c = 1:length(classes)
      if classes(c) == own
        continue
      end
      miss = nearestLocal(distances(i, :), find(labels == classes(c)), k);
      if ~isempty(miss) && missScale > 0
        factor = opts.Prior(c) / missScale;
        weights = weights + factor * meanDiffLocal(featureDiff, i, miss) / length(updates);
      end
    end
  end
end
%=============================================================================
function weights = regressionWeightsLocal(featureDiff, y, k, opts)
  y = double(y(:));
  nobs = length(y);
  nvars = size(featureDiff, 3);
  weights = zeros(1, nvars);
  distances = predictorDistancesLocal(featureDiff);
  responseRange = max(y) - min(y);
  if responseRange == 0
    return
  end
  updates = opts.Updates;
  sigma = opts.Sigma;
  if isempty(sigma)
    sigma = median(distances(distances > 0));
    if isempty(sigma) || sigma <= 0
      sigma = 1;
    end
  end
  for u = 1:length(updates)
    i = updates(u);
    neighbors = nearestLocal(distances(i, :), setdiff(1:nobs, i), k);
    if isempty(neighbors)
      continue
    end
    responseDiff = abs(y(i) - y(neighbors)) / responseRange;
    kernel = exp(-distances(i, neighbors) .^ 2 / (2 * sigma ^ 2));
    kernel = kernel(:)' / max(sum(kernel), realmin);
    for j = 1:nvars
      fd = featureDiff(i, neighbors, j);
      weights(j) = weights(j) + sum(kernel .* responseDiff(:)' .* fd(:)') / length(updates);
      weights(j) = weights(j) - sum(kernel .* (1 - responseDiff(:)') .* fd(:)') / length(updates);
    end
  end
end
%=============================================================================
function d = predictorDistancesLocal(featureDiff)
  d = sqrt(sum(featureDiff .^ 2, 3));
end
%=============================================================================
function neighbors = nearestLocal(distances, candidates, k)
  if isempty(candidates)
    neighbors = [];
    return
  end
  [~, order] = sort(distances(candidates), 'ascend');
  count = min(k, length(order));
  neighbors = candidates(order(1:count));
end
%=============================================================================
function d = meanDiffLocal(featureDiff, i, neighbors)
  nvars = size(featureDiff, 3);
  if isempty(neighbors)
    d = zeros(1, nvars);
    return
  end
  d = zeros(1, nvars);
  for j = 1:nvars
    values = featureDiff(i, neighbors, j);
    d(j) = mean(values(:));
  end
end
%=============================================================================
function labels = labelsLocal(y)
  if isnumeric(y) || islogical(y)
    [~, ~, labels] = unique(y(:));
  elseif iscell(y)
    [~, ~, labels] = unique(y(:));
  else
    [~, ~, labels] = unique(cellstr(y));
  end
  labels = double(labels(:));
end
%=============================================================================
function [classLabels, classes] = classIndicesLocal(y)
  classLabels = labelsLocal(y);
  classes = unique(classLabels);
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
