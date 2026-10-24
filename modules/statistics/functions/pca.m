%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [coeff, score, latent, tsquared, explained, mu] = pca(X, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 6);
  X = validateDataLocal(X);
  opts = parseOptionsLocal(convertStringToCharArgs(varargin), size(X, 2));
  [workX, rowMask, mu, rowWeights] = prepareRowsLocal(X, opts);
  [workX, variableScale] = applyVariableWeightsLocal(workX, opts);
  if strcmp(opts.Algorithm, 'eig') || strcmp(opts.Rows, 'pairwise')
    [coeff, latent] = pcaEigLocal(workX, opts.Centered, opts.Rows);
  else
    [coeff, latent] = pcaSvdLocal(workX, opts.Centered, rowWeights, opts.Economy);
  end
  coeff = coeff ./ repmat(variableScale(:), 1, size(coeff, 2));
  coeff = normalizeColumnsLocal(coeff);
  coeff = orientColumnsLocal(coeff);
  [coeff, latent] = selectComponentsLocal(coeff, latent, opts.NumComponents);
  score = scoreLocal(X, coeff, mu, rowMask, opts.Centered);
  tsquared = tsquaredLocal(score, latent);
  explained = explainedLocal(latent);
end
%=============================================================================
function X = validateDataLocal(X)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  X = double(X);
  if isempty(X) || size(X, 2) == 0
    error('Nelson:statistics:xLeastOneVariable', 'X must have at least one variable.');
  end
end
%=============================================================================
function opts = parseOptionsLocal(args, nvars)
  opts = struct('Algorithm', 'svd', 'Centered', true, 'Economy', true, ...
    'NumComponents', [], 'Rows', 'complete', 'Weights', [], ...
    'VariableWeights', []);
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case 'algorithm'
        opts.Algorithm = parseChoiceLocal(value, {'svd', 'eig', 'als'}, 'Algorithm');
      case 'centered'
        opts.Centered = parseLogicalScalarLocal(value, 'Centered');
      case 'economy'
        opts.Economy = parseLogicalScalarLocal(value, 'Economy');
      case 'numcomponents'
        opts.NumComponents = parseNumComponentsLocal(value, nvars);
      case 'rows'
        opts.Rows = parseChoiceLocal(value, {'complete', 'all', 'pairwise'}, 'Rows');
      case 'weights'
        opts.Weights = parseWeightsLocal(value);
      case 'variableweights'
        opts.VariableWeights = value;
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
  end
  if strcmp(opts.Algorithm, 'als')
    error('Nelson:statistics:unsupportedAlgorithm', 'Unsupported %s.', 'algorithm');
  end
end
%=============================================================================
function value = parseChoiceLocal(value, choices, name)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
  value = lower(char(value));
  if ~any(strcmp(value, choices))
    error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
  end
end
%=============================================================================
function value = parseLogicalScalarLocal(value, name)
  if islogical(value) && isscalar(value)
    value = value ~= 0;
  elseif isnumeric(value) && isscalar(value) && (value == 0 || value == 1)
    value = value ~= 0;
  else
    error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', name);
  end
end
%=============================================================================
function value = parseNumComponentsLocal(value, nvars)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value >= 0 && floor(value) == value && value <= nvars)
    error('Nelson:statistics:numComponentsIntegerScalarBetween0', 'NumComponents must be an integer scalar between 0 and the number of variables.');
  end
  value = double(value);
end
%=============================================================================
function weights = parseWeightsLocal(value)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && ~isempty(value))
    error('Nelson:statistics:weightsRealNumericVector', '%s must be a real numeric vector.', 'Weights');
  end
  weights = double(value(:));
  if any(~isfinite(weights)) || any(weights < 0) || sum(weights) <= 0
    error('Nelson:statistics:weightsContainNonnegativeFiniteValues:valueContainNonnegativeFiniteValuesPositive', '%s must contain nonnegative finite values with positive sum.', 'Weights');
  end
end
%=============================================================================
function [workX, rowMask, mu, rowWeights] = prepareRowsLocal(X, opts)
  rowMask = true(size(X, 1), 1);
  if strcmp(opts.Rows, 'all') && any(isnan(X(:)))
    error('Nelson:statistics:rawDataContainsNaNWhile', 'Raw data contains NaN while Rows is all.');
  elseif strcmp(opts.Rows, 'complete')
    rowMask = ~any(isnan(X), 2);
  elseif strcmp(opts.Rows, 'pairwise')
    if ~isempty(opts.Weights)
      error('Nelson:statistics:weightsSupportedPairwiseRows', 'Weights are not supported with pairwise rows.');
    end
    rowMask = ~any(isnan(X), 2);
    workX = X;
    mu = meanOmitNaNLocal(X, opts.Centered);
    rowWeights = [];
    return
  end
  workX = X(rowMask, :);
  if isempty(workX)
    error('Nelson:statistics:xContainLeastOneComplete', 'X must contain at least one complete row.');
  end
  rowWeights = [];
  if ~isempty(opts.Weights)
    if length(opts.Weights) ~= size(X, 1)
      error('Nelson:statistics:weightsOneValueObservation', '%s must have one value per observation.', 'Weights');
    end
    rowWeights = opts.Weights(rowMask);
  end
  mu = meanLocal(workX, rowWeights, opts.Centered);
  if opts.Centered
    workX = workX - mu;
  end
end
%=============================================================================
function mu = meanOmitNaNLocal(X, centered)
  if ~centered
    mu = zeros(1, size(X, 2));
    return
  end
  mu = zeros(1, size(X, 2));
  for k = 1:size(X, 2)
    column = X(:, k);
    column = column(isfinite(column));
    if isempty(column)
      error('Nelson:statistics:variableContainLeastOneFinite', '%s must contain at least one finite value.', 'Each variable');
    end
    mu(k) = mean(column);
  end
end
%=============================================================================
function mu = meanLocal(X, weights, centered)
  if ~centered
    mu = zeros(1, size(X, 2));
  elseif isempty(weights)
    mu = mean(X, 1);
  else
    weights = weights(:) / sum(weights);
    mu = weights' * X;
  end
end
%=============================================================================
function [X, scale] = applyVariableWeightsLocal(X, opts)
  scale = ones(1, size(X, 2));
  value = opts.VariableWeights;
  if isempty(value)
    return
  end
  if ischar(value) || (isstring(value) && isscalar(value))
    if ~strcmpi(char(value), 'variance')
      error('Nelson:statistics:unsupportedVariableWeights', 'Unsupported %s.', 'variable weights');
    end
    scale = std(X, 0, 1);
  else
    if ~(isnumeric(value) && isreal(value) && isvector(value) && length(value) == size(X, 2))
      error('Nelson:statistics:variableWeightsMatchNumberVariables', 'VariableWeights must match the number of variables.');
    end
    scale = 1 ./ sqrt(double(value(:))');
  end
  scale(~isfinite(scale) | scale <= 0) = 1;
  X = X ./ repmat(scale, size(X, 1), 1);
end
%=============================================================================
function [coeff, latent] = pcaSvdLocal(X, centered, weights, economy)
  if ~isempty(weights)
    X = X .* sqrt(weights(:) / mean(weights));
  end
  if size(X, 2) > size(X, 1) || ~economy
    [coeff, latent] = pcaEigLocal(X, centered, 'complete');
    if economy
      [coeff, latent] = truncateEconomyLocal(coeff, latent, size(X, 1), centered, economy);
    end
    return
  end
  if economy
    [~, S, coeff] = svd(X, 'econ');
  else
    [~, S, coeff] = svd(X);
  end
  s = diag(S);
  denom = max(size(X, 1) - double(centered), 1);
  latent = (s .^ 2) / denom;
  [coeff, latent] = truncateEconomyLocal(coeff, latent, size(X, 1), centered, economy);
end
%=============================================================================
function [coeff, latent] = truncateEconomyLocal(coeff, latent, nobs, centered, economy)
  if ~economy
    if length(latent) < size(coeff, 2)
      latent = [latent; zeros(size(coeff, 2) - length(latent), 1)];
    end
    return
  end
  ncomp = min(size(coeff, 2), max(nobs - double(centered), 0));
  coeff = coeff(:, 1:ncomp);
  latent = latent(1:ncomp);
end
%=============================================================================
function [coeff, latent] = pcaEigLocal(X, centered, rowsMode)
  if strcmp(rowsMode, 'pairwise')
    C = pairwiseCovLocal(X, centered);
  else
    denom = max(size(X, 1) - double(centered), 1);
    C = (X' * X) / denom;
  end
  [V, D] = eig((C + C') / 2);
  latent = real(diag(D));
  [latent, order] = sort(latent, 'descend');
  coeff = real(V(:, order));
  latent(latent < 0 & latent > -sqrt(eps)) = 0;
end
%=============================================================================
function C = pairwiseCovLocal(X, centered)
  nvars = size(X, 2);
  C = zeros(nvars, nvars);
  for i = 1:nvars
    for j = i:nvars
      keep = isfinite(X(:, i)) & isfinite(X(:, j));
      xi = X(keep, i);
      xj = X(keep, j);
      if centered
        xi = xi - mean(xi);
        xj = xj - mean(xj);
      end
      denom = max(length(xi) - double(centered), 1);
      C(i, j) = (xi' * xj) / denom;
      C(j, i) = C(i, j);
    end
  end
end
%=============================================================================
function coeff = normalizeColumnsLocal(coeff)
  for k = 1:size(coeff, 2)
    n = norm(coeff(:, k));
    if n > 0
      coeff(:, k) = coeff(:, k) / n;
    end
  end
end
%=============================================================================
function coeff = orientColumnsLocal(coeff)
  for k = 1:size(coeff, 2)
    [~, idx] = max(abs(coeff(:, k)));
    if coeff(idx, k) < 0
      coeff(:, k) = -coeff(:, k);
    end
  end
end
%=============================================================================
function [coeff, latent] = selectComponentsLocal(coeff, latent, numComponents)
  if isempty(numComponents)
    return
  end
  coeff = coeff(:, 1:numComponents);
  latent = latent(1:numComponents);
end
%=============================================================================
function score = scoreLocal(X, coeff, mu, rowMask, centered)
  score = NaN(size(X, 1), size(coeff, 2));
  complete = rowMask;
  Xwork = X(complete, :);
  if centered
    Xwork = Xwork - mu;
  end
  score(complete, :) = Xwork * coeff;
end
%=============================================================================
function tsquared = tsquaredLocal(score, latent)
  tsquared = NaN(size(score, 1), 1);
  if isempty(latent)
    tsquared(:) = 0;
    return
  end
  usable = latent(:)' > eps(max(latent));
  if ~any(usable)
    tsquared(~any(isnan(score), 2)) = 0;
    return
  end
  usableLatent = latent(usable)';
  for k = 1:size(score, 1)
    row = score(k, usable);
    if ~any(isnan(row))
      tsquared(k) = sum((row .^ 2) ./ usableLatent);
    end
  end
end
%=============================================================================
function explained = explainedLocal(latent)
  total = sum(latent);
  if total > 0
    explained = 100 * latent(:) / total;
  else
    explained = zeros(size(latent(:)));
  end
end
%=============================================================================
