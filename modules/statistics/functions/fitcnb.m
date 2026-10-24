%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitcnb(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, yIndex, classNames] = __classification_training_data__(X, Y);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptionsLocal(varargin, classNames, size(X, 2), size(X, 1));
  [yIndex, classNames] = __reorder_class_names__(yIndex, classNames, opts.ClassNames);
  prior = priorLocal(opts.Prior, yIndex, length(classNames));
  [mu, sigma] = fitNormalParametersLocal(X, yIndex, length(classNames), opts.Weights);
  mdl = ClassificationNaiveBayes();
  mdl.ClassNames = classNames;
  mdl.Prior = prior;
  mdl.Mu = mu;
  mdl.Sigma = sigma;
  mdl.DistributionNames = opts.DistributionNames;
  mdl.DistributionParameters = distributionParametersLocal(mu, sigma);
  mdl.NumObservations = size(X, 1);
  mdl.NumPredictors = size(X, 2);
end
%=============================================================================
function opts = parseOptionsLocal(args, classNames, p, n)
  opts = struct('ClassNames', {classNames}, 'Prior', 'empirical', ...
    'DistributionNames', {repmat({'normal'}, 1, p)}, 'Weights', []);
  for k = 1:2:length(args)
    name = __option_name__(args{k}, {'classnames', 'prior', 'distributionnames', 'weights'});
    value = args{k + 1};
    switch name
      case 'classnames'
        opts.ClassNames = __class_names__(value, classNames);
      case 'prior'
        opts.Prior = priorOptionLocal(value, length(classNames));
      case 'distributionnames'
        opts.DistributionNames = distributionNamesLocal(value, p);
      case 'weights'
        opts.Weights = weightsLocal(value, n);
    end
  end
end
%=============================================================================
function [mu, sigma] = fitNormalParametersLocal(X, yIndex, kClasses, weights)
  [n, p] = size(X);
  if isempty(weights)
    weights = ones(n, 1);
  end
  mu = zeros(kClasses, p);
  sigma = zeros(kClasses, p);
  for j = 1:kClasses
    rows = yIndex == j;
    w = weights(rows);
    xj = X(rows, :);
    wsum = sum(w);
    mu(j, :) = (w(:)' * xj) / wsum;
    centered = xj - repmat(mu(j, :), size(xj, 1), 1);
    sigma(j, :) = sqrt((w(:)' * (centered .^ 2)) / wsum);
  end
  if any(sigma(:) == 0 | isnan(sigma(:)))
    error('Nelson:statistics:normalDistributionFitBecauseOne', 'A normal distribution cannot be fit because one class has zero variance.');
  end
end
%=============================================================================
function params = distributionParametersLocal(mu, sigma)
  params = cell(size(mu, 1), size(mu, 2));
  for i = 1:size(mu, 1)
    for j = 1:size(mu, 2)
      params{i, j} = [mu(i, j), sigma(i, j)];
    end
  end
end
%=============================================================================
function prior = priorLocal(option, yIndex, kClasses)
  if ischar(option)
    switch option
      case 'empirical'
        counts = zeros(1, kClasses);
        for j = 1:kClasses
          counts(j) = sum(yIndex == j);
        end
        prior = counts / sum(counts);
      otherwise
        prior = ones(1, kClasses) / kClasses;
    end
  else
    prior = double(option(:).');
    prior = prior / sum(prior);
  end
end
%=============================================================================
function value = priorOptionLocal(value, kClasses)
  if ischar(value) || (isstring(value) && isscalar(value))
    text = lower(char(value));
    choices = {'empirical', 'uniform'};
    matches = choices(startsWith(choices, text));
    if length(matches) ~= 1
      error('Nelson:statistics:invalidPriorValue', 'Invalid %s value.', 'Prior');
    end
    value = matches{1};
  elseif isnumeric(value) && isreal(value) && isvector(value) && length(value) == kClasses ...
      && all(isfinite(value)) && all(value >= 0) && sum(value) > 0
    value = double(value(:).');
  else
    error('Nelson:statistics:invalidPriorValue', 'Invalid %s value.', 'Prior');
  end
end
%=============================================================================
function value = distributionNamesLocal(value, p)
  if ischar(value) || (isstring(value) && isscalar(value))
    value = repmat({char(value)}, 1, p);
  elseif iscellstr(value) && (length(value) == 1 || length(value) == p)
    value = value(:).';
    if length(value) == 1
      value = repmat(value, 1, p);
    end
  else
    error('Nelson:statistics:distributionNamesNormalCellArrayNormal', 'DistributionNames must be normal or a cell array of normal values.');
  end
  for k = 1:length(value)
    if ~strcmpi(value{k}, 'normal')
      error('Nelson:statistics:normalPredictorDistributionsSupported', 'Only normal predictor distributions are supported.');
    end
    value{k} = 'normal';
  end
end
%=============================================================================
function value = weightsLocal(value, n)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && all(isfinite(value)) ...
      && length(value) == n && all(value >= 0) && sum(value) > 0)
    error('Nelson:statistics:weightsNonnegativeFiniteNumericVector', 'Weights must be a nonnegative finite numeric vector with positive sum.');
  end
  value = double(value(:));
end
%=============================================================================
