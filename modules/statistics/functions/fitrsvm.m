%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitrsvm(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, y] = __regression_training_data__(X, Y);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptionsLocal(varargin, X);
  [Xfit, mu, sigma] = standardizeTrainingLocal(X, opts.Standardize);
  fit = fitKernelRegressionLocal(Xfit, y, opts);
  mdl = RegressionSVM();
  mdl.SupportVectors = Xfit;
  mdl.Alpha = fit.Alpha;
  mdl.Bias = fit.Bias;
  mdl.KernelFunction = opts.KernelFunction;
  mdl.KernelScale = opts.KernelScale;
  mdl.PolynomialOrder = opts.PolynomialOrder;
  mdl.BoxConstraint = opts.BoxConstraint;
  mdl.Epsilon = opts.Epsilon;
  mdl.Standardize = opts.Standardize;
  mdl.Mu = mu;
  mdl.Sigma = sigma;
  mdl.PredictorNames = opts.PredictorNames;
  mdl.ResponseName = opts.ResponseName;
  mdl.NumObservations = size(X, 1);
  mdl.NumPredictors = size(X, 2);
end
%=============================================================================
function opts = parseOptionsLocal(args, X)
  opts = struct('KernelFunction', 'linear', 'KernelScale', kernelScaleAutoLocal(X), ...
    'PolynomialOrder', 3, 'BoxConstraint', 1, 'Epsilon', 0.1, 'Standardize', false, ...
    'PredictorNames', {__default_predictor_names__(size(X, 2))}, 'ResponseName', 'y');
  for k = 1:2:length(args)
    name = __option_name__(args{k}, {'kernelfunction', 'kernelscale', 'polynomialorder', ...
      'boxconstraint', 'epsilon', 'standardize', 'predictornames', 'responsename'});
    value = args{k + 1};
    switch name
      case 'kernelfunction'
        opts.KernelFunction = kernelFunctionLocal(value);
      case 'kernelscale'
        opts.KernelScale = kernelScaleLocal(value, X);
      case 'polynomialorder'
        opts.PolynomialOrder = positiveIntegerLocal(value, 'PolynomialOrder');
      case 'boxconstraint'
        opts.BoxConstraint = positiveScalarLocal(value, 'BoxConstraint');
      case 'epsilon'
        opts.Epsilon = nonnegativeScalarLocal(value, 'Epsilon');
      case 'standardize'
        opts.Standardize = logicalScalarLocal(value, 'Standardize');
      case 'predictornames'
        opts.PredictorNames = __predictor_names__(value, size(X, 2));
      case 'responsename'
        opts.ResponseName = __response_name__(value);
    end
  end
end
%=============================================================================
function fit = fitKernelRegressionLocal(X, y, opts)
  yMean = mean(y);
  centered = y - yMean;
  K = kernelMatrixLocal(X, X, opts);
  lambda = 1 / max(opts.BoxConstraint, eps);
  alpha = (K + lambda * eye(size(K))) \ centered;
  fit = struct('Alpha', alpha, 'Bias', yMean);
end
%=============================================================================
function K = kernelMatrixLocal(A, B, opts)
  switch opts.KernelFunction
    case 'gaussian'
      a2 = sum(A .^ 2, 2);
      b2 = sum(B .^ 2, 2).';
      dist2 = repmat(a2, 1, size(B, 1)) + repmat(b2, size(A, 1), 1) - 2 * (A * B.');
      K = exp(-max(dist2, 0) / (2 * opts.KernelScale ^ 2));
    case 'polynomial'
      K = (1 + A * B.') .^ opts.PolynomialOrder;
    otherwise
      K = A * B.';
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
function name = kernelFunctionLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'KernelFunction');
  end
  text = lower(char(value));
  names = {'linear', 'gaussian', 'rbf', 'polynomial'};
  exact = names(strcmp(names, text));
  if length(exact) == 1
    name = exact{1};
  else
    matches = names(startsWith(names, text));
    if length(matches) ~= 1
      error('Nelson:statistics:invalidKernelFunctionValue', 'Invalid %s value.', 'KernelFunction');
    end
    name = matches{1};
  end
  if strcmp(name, 'rbf')
    name = 'gaussian';
  end
end
%=============================================================================
function value = kernelScaleLocal(value, X)
  if ischar(value) || (isstring(value) && isscalar(value))
    if strcmpi(char(value), 'auto')
      value = kernelScaleAutoLocal(X);
      return
    end
  end
  value = positiveScalarLocal(value, 'KernelScale');
end
%=============================================================================
function value = kernelScaleAutoLocal(X)
  value = sqrt(size(X, 2));
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
function value = nonnegativeScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value >= 0)
    error('Nelson:statistics:mustBeNonnegative', '%s must be a nonnegative scalar.', name);
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
