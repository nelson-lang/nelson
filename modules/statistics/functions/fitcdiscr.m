%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitcdiscr(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, yIndex, classNames] = __classification_training_data__(X, Y);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptionsLocal(varargin, classNames);
  [yIndex, classNames] = __reorder_class_names__(yIndex, classNames, opts.ClassNames);
  prior = priorLocal(opts.Prior, yIndex, length(classNames));
  [mu, Sigma] = fitDiscriminantParametersLocal(X, yIndex, length(classNames), opts);
  mdl = ClassificationDiscriminant();
  mdl.ClassNames = classNames;
  mdl.Prior = prior;
  mdl.Mu = mu;
  mdl.Sigma = Sigma;
  mdl.DiscrimType = opts.DiscrimType;
  mdl.Gamma = opts.Gamma;
  mdl.Delta = opts.Delta;
  mdl.NumObservations = size(X, 1);
  mdl.NumPredictors = size(X, 2);
end
%=============================================================================
function opts = parseOptionsLocal(args, classNames)
  opts = struct('ClassNames', {classNames}, 'Prior', 'empirical', ...
    'DiscrimType', 'linear', 'Gamma', 0, 'Delta', 0);
  for k = 1:2:length(args)
    name = __option_name__(args{k}, {'classnames', 'prior', 'discrimtype', 'gamma', 'delta'});
    value = args{k + 1};
    switch name
      case 'classnames'
        opts.ClassNames = __class_names__(value, classNames);
      case 'prior'
        opts.Prior = priorOptionLocal(value, length(classNames));
      case 'discrimtype'
        opts.DiscrimType = discrimTypeLocal(value);
      case 'gamma'
        opts.Gamma = scalarRangeLocal(value, 'Gamma');
      case 'delta'
        opts.Delta = nonnegativeScalarLocal(value, 'Delta');
    end
  end
end
%=============================================================================
function [mu, Sigma] = fitDiscriminantParametersLocal(X, yIndex, kClasses, opts)
  p = size(X, 2);
  mu = zeros(kClasses, p);
  classSigma = zeros(p, p, kClasses);
  counts = zeros(1, kClasses);
  for j = 1:kClasses
    xj = X(yIndex == j, :);
    counts(j) = size(xj, 1);
    if counts(j) <= 1
      error('Nelson:statistics:classContainLeastTwoObservations', 'Each class must contain at least two observations.');
    end
    mu(j, :) = mean(xj, 1);
    centered = xj - repmat(mu(j, :), counts(j), 1);
    classSigma(:, :, j) = centered' * centered / max(counts(j) - 1, 1);
  end
  Sigma = covarianceForTypeLocal(classSigma, counts, opts);
end
%=============================================================================
function Sigma = covarianceForTypeLocal(classSigma, counts, opts)
  pooled = pooledCovarianceLocal(classSigma, counts);
  switch opts.DiscrimType
    case 'linear'
      Sigma = regularizeCovarianceLocal(pooled, opts);
      Sigma = reshape(Sigma, size(Sigma, 1), size(Sigma, 2), 1);
    case 'diaglinear'
      Sigma = diag(diag(regularizeCovarianceLocal(pooled, opts)));
      Sigma = reshape(Sigma, size(Sigma, 1), size(Sigma, 2), 1);
    case 'quadratic'
      Sigma = regularizePagesLocal(classSigma, opts, false);
    otherwise
      Sigma = regularizePagesLocal(classSigma, opts, true);
  end
end
%=============================================================================
function S = pooledCovarianceLocal(classSigma, counts)
  totalDf = sum(counts - 1);
  S = zeros(size(classSigma, 1), size(classSigma, 2));
  for j = 1:length(counts)
    S = S + classSigma(:, :, j) * (counts(j) - 1);
  end
  S = S / totalDf;
end
%=============================================================================
function pages = regularizePagesLocal(pages, opts, diagonalOnly)
  for j = 1:size(pages, 3)
    S = regularizeCovarianceLocal(pages(:, :, j), opts);
    if diagonalOnly
      S = diag(diag(S));
    end
    pages(:, :, j) = S;
  end
end
%=============================================================================
function S = regularizeCovarianceLocal(S, opts)
  if opts.Gamma > 0
    D = diag(diag(S));
    S = (1 - opts.Gamma) * S + opts.Gamma * D;
  end
  scale = max(mean(diag(S)), eps);
  S = S + (opts.Delta + 1e-9 * scale) * eye(size(S));
end
%=============================================================================
function prior = priorLocal(option, yIndex, kClasses)
  if ischar(option)
    if strcmp(option, 'uniform')
      prior = ones(1, kClasses) / kClasses;
    else
      counts = zeros(1, kClasses);
      for j = 1:kClasses
        counts(j) = sum(yIndex == j);
      end
      prior = counts / sum(counts);
    end
  else
    prior = double(option(:).');
    prior = prior / sum(prior);
  end
end
%=============================================================================
function value = discrimTypeLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'DiscrimType');
  end
  text = lower(char(value));
  names = {'linear', 'quadratic', 'diaglinear', 'diagquadratic'};
  exact = names(strcmp(names, text));
  if length(exact) == 1
    value = exact{1};
    return
  end
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidDiscrimTypeValue', 'Invalid %s value.', 'DiscrimType');
  end
  value = matches{1};
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
function value = scalarRangeLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value >= 0 && value <= 1)
    error('Nelson:statistics:mustBeInRange:valueAScalarRange01', '%s must be a scalar in the range [0,1].', name);
  end
  value = double(value);
end
%=============================================================================
function value = nonnegativeScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value >= 0)
    error('Nelson:statistics:mustBeNonnegative', '%s must be a nonnegative scalar.', name);
  end
  value = double(value);
end
%=============================================================================
