%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitrknn(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, y] = __regression_training_data__(X, Y);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptionsLocal(varargin, X);
  [Xfit, mu, sigma] = standardizeTrainingLocal(X, opts.Standardize);
  mdl = RegressionKNN();
  mdl.X = Xfit;
  mdl.Y = y;
  mdl.NumNeighbors = opts.NumNeighbors;
  mdl.Distance = opts.Distance;
  mdl.DistanceWeight = opts.DistanceWeight;
  mdl.Standardize = opts.Standardize;
  mdl.Mu = mu;
  mdl.Sigma = sigma;
  mdl.P = opts.P;
  mdl.Scale = opts.Scale;
  mdl.PredictorNames = opts.PredictorNames;
  mdl.ResponseName = opts.ResponseName;
  mdl.NumObservations = size(X, 1);
  mdl.NumPredictors = size(X, 2);
end
%=============================================================================
function opts = parseOptionsLocal(args, X)
  opts = struct('NumNeighbors', 1, 'Distance', 'euclidean', 'DistanceWeight', 'equal', ...
    'Standardize', false, 'P', 2, 'Scale', [], ...
    'PredictorNames', {__default_predictor_names__(size(X, 2))}, 'ResponseName', 'y');
  for k = 1:2:length(args)
    name = __option_name__(args{k}, {'numneighbors', 'distance', 'distanceweight', ...
      'standardize', 'p', 'scale', 'predictornames', 'responsename'});
    value = args{k + 1};
    switch name
      case 'numneighbors'
        opts.NumNeighbors = positiveIntegerLocal(value, 'NumNeighbors');
      case 'distance'
        opts.Distance = distanceNameLocal(value);
      case 'distanceweight'
        opts.DistanceWeight = distanceWeightLocal(value);
      case 'standardize'
        opts.Standardize = logicalScalarLocal(value, 'Standardize');
      case 'p'
        opts.P = positiveScalarLocal(value, 'P');
      case 'scale'
        opts.Scale = scaleVectorLocal(value, size(X, 2));
      case 'predictornames'
        opts.PredictorNames = __predictor_names__(value, size(X, 2));
      case 'responsename'
        opts.ResponseName = __response_name__(value);
    end
  end
  if opts.NumNeighbors > size(X, 1)
    error('Nelson:statistics:numNeighborsExceedNumberTrainingObservations', 'NumNeighbors must not exceed the number of training observations.');
  end
end
%=============================================================================
function [Xfit, mu, sigma] = standardizeTrainingLocal(X, doStandardize)
  mu = zeros(1, size(X, 2));
  sigma = ones(1, size(X, 2));
  Xfit = X;
  if doStandardize
    mu = mean(X, 1);
    centered = X - repmat(mu, size(X, 1), 1);
    sigma = sqrt(sum(centered .^ 2, 1) / max(size(X, 1) - 1, 1));
    sigma(sigma == 0 | isnan(sigma)) = 1;
    Xfit = centered ./ repmat(sigma, size(X, 1), 1);
  end
end
%=============================================================================
function name = distanceNameLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Distance');
  end
  text = lower(char(value));
  names = {'euclidean', 'cityblock', 'minkowski', 'chebychev', 'cosine', ...
    'correlation', 'spearman', 'hamming', 'jaccard', 'seuclidean'};
  exact = names(strcmp(names, text));
  if length(exact) == 1
    name = exact{1};
    return
  end
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidDistance', 'Invalid %s value.', 'Distance');
  end
  name = matches{1};
end
%=============================================================================
function name = distanceWeightLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'DistanceWeight');
  end
  text = lower(char(value));
  names = {'equal', 'inverse', 'squaredinverse'};
  exact = names(strcmp(names, text));
  if length(exact) == 1
    name = exact{1};
    return
  end
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidDistanceWeightValue', 'Invalid %s value.', 'DistanceWeight');
  end
  name = matches{1};
end
%=============================================================================
function value = scaleVectorLocal(value, p)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && length(value) == p ...
      && all(isfinite(value)) && all(value > 0))
    error('Nelson:statistics:scalePositiveFiniteVectorOne', 'Scale must be a positive finite vector with one value per predictor.');
  end
  value = double(value(:).');
end
%=============================================================================
function value = positiveIntegerLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value > 0 && value == fix(value))
    error('Nelson:statistics:mustBePositiveInteger', '%s must be a positive integer scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = positiveScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0)
    error('Nelson:statistics:mustBePositive', '%s must be a positive scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = logicalScalarLocal(value, name)
  if islogical(value) && isscalar(value)
    return
  end
  if isnumeric(value) && isscalar(value) && (value == 0 || value == 1)
    value = logical(value);
    return
  end
  error('Nelson:statistics:mustBeTrueOrFalse', '%s must be true or false.', name);
end
%=============================================================================
