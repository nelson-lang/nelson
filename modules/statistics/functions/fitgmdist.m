%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function gm = fitgmdist(X, k, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  X = __gmm_check_data__(X);
  k = positiveInteger(k, sprintf(_('%s must be a positive integer scalar.'), 'K'));
  if k > size(X, 1)
    error('Nelson:statistics:kTooLarge', 'K must not exceed the number of rows in X.');
  end
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptions(varargin, size(X, 2));
  best = [];
  for r = 1:opts.Replicates
    current = fitOneReplicate(X, k, opts, r);
    if isempty(best) || current.NegativeLogLikelihood < best.NegativeLogLikelihood
      best = current;
    end
  end
  gm = best;
end
%=============================================================================
function gm = fitOneReplicate(X, k, opts, replicate)
  [mu, Sigma, p] = initializeModel(X, k, opts, replicate);
  oldNlogL = Inf;
  converged = false;
  for iter = 1:opts.MaxIter
    [P, nlogL] = __gmm_responsibilities__(X, mu, Sigma, p);
    [mu, Sigma, p] = maximizeModel(X, P, opts);
    if isfinite(oldNlogL) && abs(oldNlogL - nlogL) <= opts.TolFun * (1 + abs(oldNlogL))
      converged = true;
      break
    end
    oldNlogL = nlogL;
  end
  [~, nlogL] = __gmm_responsibilities__(X, mu, Sigma, p);
  gm = gmdistribution(mu, Sigma, p);
  gm.NegativeLogLikelihood = nlogL;
  gm.NumIterations = iter;
  gm.Converged = converged;
  gm.RegularizationValue = opts.RegularizationValue;
  gm.CovarianceType = opts.CovarianceType;
  gm.SharedCovariance = opts.SharedCovariance;
  gm.AIC = 2 * modelParameterCount(gm) + 2 * nlogL;
  gm.BIC = modelParameterCount(gm) * log(size(X, 1)) + 2 * nlogL;
end
%=============================================================================
function opts = parseOptions(args, variables)
  opts = defaultOptions();
  for pos = 1:2:length(args)
    name = optionName(args{pos});
    value = args{pos + 1};
    switch name
      case 'regularizationvalue'
        opts.RegularizationValue = nonnegativeScalar(value, sprintf(_('%s must be nonnegative.'), 'RegularizationValue'));
      case 'replicates'
        opts.Replicates = positiveInteger(value, sprintf(_('%s must be a positive integer scalar.'), 'Replicates'));
      case 'start'
        opts.Start = value;
      case 'options'
        opts = mergeStatOptions(opts, value);
      case 'covariancetype'
        opts.CovarianceType = covarianceType(value);
      case 'sharedcovariance'
        opts.SharedCovariance = logicalScalar(value, sprintf(_('%s must be true or false.'), 'SharedCovariance'));
      case 'probabilitytolerance'
        opts.ProbabilityTolerance = nonnegativeScalar(value, sprintf(_('%s must be nonnegative.'), 'ProbabilityTolerance'));
      case 'maxiter'
        opts.MaxIter = positiveInteger(value, sprintf(_('%s must be a positive integer scalar.'), 'MaxIter'));
      case 'tolfun'
        opts.TolFun = positiveScalar(value, sprintf(_('%s must be a positive scalar.'), 'TolFun'));
    end
  end
  if strcmp(opts.CovarianceType, 'diagonal') && variables == 0
    error('Nelson:statistics:xLeastOneColumn', 'X must have at least one column.');
  end
end
%=============================================================================
function opts = defaultOptions()
  opts = struct('RegularizationValue', 1e-6, 'Replicates', 1, 'Start', 'plus', ...
    'MaxIter', 100, 'TolFun', 1e-6, 'CovarianceType', 'full', ...
    'SharedCovariance', false, 'ProbabilityTolerance', 0);
end
%=============================================================================
function opts = mergeStatOptions(opts, value)
  if ~isempty(value) && ~isstruct(value)
    error('Nelson:statistics:optionsStructure', 'Options must be a structure.');
  end
  if isempty(value)
    return
  end
  if isfield(value, 'MaxIter') && ~isempty(value.MaxIter)
    opts.MaxIter = positiveInteger(value.MaxIter, sprintf(_('%s must be a positive integer scalar.'), 'MaxIter'));
  end
  if isfield(value, 'TolFun') && ~isempty(value.TolFun)
    opts.TolFun = positiveScalar(value.TolFun, sprintf(_('%s must be a positive scalar.'), 'TolFun'));
  end
end
%=============================================================================
function [mu, Sigma, p] = initializeModel(X, k, opts, replicate)
  start = opts.Start;
  if isnumeric(start)
    labels = labelsFromNumericStart(X, k, start, replicate);
  else
    validateStartText(start);
    labels = deterministicStart(X, k);
  end
  [mu, Sigma, p] = maximizeModel(X, labelsToPosterior(labels, k), opts);
end
%=============================================================================
function labels = labelsFromNumericStart(X, k, start, replicate)
  if isvector(start) && length(start) == size(X, 1)
    labels = start(:);
  elseif ndims(start) == 2 && isequal(size(start), [k size(X, 2)])
    labels = nearestCenterLabels(X, start);
  elseif ndims(start) == 3 && size(start, 1) == k && size(start, 2) == size(X, 2)
    page = min(replicate, size(start, 3));
    labels = nearestCenterLabels(X, start(:, :, page));
  else
    error('Nelson:statistics:invalidStartValue', 'Invalid %s value.', 'Start');
  end
  if any(labels < 1) || any(labels > k) || any(floor(labels) ~= labels)
    error('Nelson:statistics:invalidStartValue', 'Invalid %s value.', 'Start');
  end
end
%=============================================================================
function labels = deterministicStart(X, k)
  [~, order] = sort(X(:, 1));
  labels = zeros(size(X, 1), 1);
  for pos = 1:length(order)
    labels(order(pos)) = mod(pos - 1, k) + 1;
  end
end
%=============================================================================
function validateStartText(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:invalidStartValue', 'Invalid %s value.', 'Start');
  end
  text = lower(char(value));
  names = {'plus', 'sample', 'randsample'};
  if ~any(strncmp(names, text, length(text)))
    error('Nelson:statistics:invalidStartValue', 'Invalid %s value.', 'Start');
  end
end
%=============================================================================
function labels = nearestCenterLabels(X, centers)
  D = pdist2(X, centers);
  [~, labels] = min(D, [], 2);
end
%=============================================================================
function P = labelsToPosterior(labels, k)
  P = zeros(length(labels), k);
  for j = 1:k
    P(:, j) = labels == j;
  end
end
%=============================================================================
function [mu, Sigma, p] = maximizeModel(X, P, opts)
  n = size(X, 1);
  d = size(X, 2);
  nk = max(sum(P, 1), realmin);
  p = nk / n;
  mu = (P' * X) ./ repmat(nk(:), 1, d);
  Sigma = covariancePages(X, P, mu, nk, opts);
end
%=============================================================================
function Sigma = covariancePages(X, P, mu, nk, opts)
  d = size(X, 2);
  k = size(mu, 1);
  Sigma = zeros(d, d, k);
  for j = 1:k
    centered = X - repmat(mu(j, :), size(X, 1), 1);
    weighted = centered .* repmat(P(:, j), 1, d);
    Sigma(:, :, j) = centered' * weighted / nk(j);
    if strcmp(opts.CovarianceType, 'diagonal')
      Sigma(:, :, j) = diag(diag(Sigma(:, :, j)));
    end
    Sigma(:, :, j) = Sigma(:, :, j) + opts.RegularizationValue * eye(d);
  end
  if opts.SharedCovariance
    Sigma = repmat(sharedCovariance(Sigma, nk), [1 1 k]);
  end
end
%=============================================================================
function S = sharedCovariance(Sigma, nk)
  S = zeros(size(Sigma, 1), size(Sigma, 2));
  for j = 1:size(Sigma, 3)
    S = S + Sigma(:, :, j) * nk(j);
  end
  S = S / sum(nk);
end
%=============================================================================
function count = modelParameterCount(gm)
  k = gm.NumComponents;
  d = gm.NumVariables;
  if strcmp(gm.CovarianceType, 'diagonal')
    covParams = d;
  else
    covParams = d * (d + 1) / 2;
  end
  if gm.SharedCovariance
    totalCovParams = covParams;
  else
    totalCovParams = k * covParams;
  end
  count = k * d + totalCovParams + k - 1;
end
%=============================================================================
function name = optionName(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  names = {'regularizationvalue', 'replicates', 'start', 'options', ...
    'covariancetype', 'sharedcovariance', 'probabilitytolerance', 'maxiter', 'tolfun'};
  matches = names(strncmp(names, text, length(text)));
  if isempty(matches)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousNameValue', 'Ambiguous name-value argument name.');
  end
  name = matches{1};
end
%=============================================================================
function value = covarianceType(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:covarianceTypeFullDiagonal', 'CovarianceType must be full or diagonal.');
  end
  text = lower(char(value));
  if strncmp('full', text, length(text))
    value = 'full';
  elseif strncmp('diagonal', text, length(text))
    value = 'diagonal';
  else
    error('Nelson:statistics:covarianceTypeFullDiagonal', 'CovarianceType must be full or diagonal.');
  end
end
%=============================================================================
function value = logicalScalar(value, message)
  if islogical(value) && isscalar(value)
    return
  end
  if isnumeric(value) && isscalar(value) && (value == 0 || value == 1)
    value = logical(value);
    return
  end
  error('Nelson:validators:mustBeLogicalScalar', message);
end
%=============================================================================
function value = positiveInteger(value, message)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && floor(value) == value && value > 0)
    error('Nelson:statistics:mustBePositiveInteger', message);
  end
  value = double(value);
end
%=============================================================================
function value = positiveScalar(value, message)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0)
    error('Nelson:statistics:mustBePositive', message);
  end
  value = double(value);
end
%=============================================================================
function value = nonnegativeScalar(value, message)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value >= 0)
    error('Nelson:statistics:mustBeNonnegative', message);
  end
  value = double(value);
end
%=============================================================================
