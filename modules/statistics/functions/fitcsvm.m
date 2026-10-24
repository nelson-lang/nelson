%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitcsvm(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, yIndex, classNames] = __classification_training_data__(X, Y, 2);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptionsLocal(varargin, X, classNames);
  [yIndex, classNames] = __reorder_class_names__(yIndex, classNames, opts.ClassNames);
  [Xfit, mu, sigma] = standardizeTrainingLocal(X, opts.Standardize);
  ySigned = -ones(length(yIndex), 1);
  ySigned(yIndex == 2) = 1;
  opts.BoxConstraints = costConstraintsLocal(yIndex, opts);
  K = kernelMatrixLocal(Xfit, Xfit, opts);
  smo = trainSmoLocal(K, ySigned, opts);
  support = smo.Alpha > 1e-8;
  mdl = ClassificationSVM();
  mdl.SupportVectors = Xfit(support, :);
  mdl.SupportVectorLabels = ySigned(support);
  mdl.Alpha = smo.Alpha(support);
  mdl.Bias = smo.Bias;
  mdl.ClassNames = classNames;
  mdl.KernelFunction = opts.KernelFunction;
  mdl.KernelScale = opts.KernelScale;
  mdl.PolynomialOrder = opts.PolynomialOrder;
  mdl.BoxConstraint = opts.BoxConstraint;
  mdl.BoxConstraints = opts.BoxConstraints(support);
  mdl.Cost = opts.Cost;
  mdl.Standardize = opts.Standardize;
  mdl.Mu = mu;
  mdl.Sigma = sigma;
  mdl.NumObservations = size(X, 1);
  mdl.NumPredictors = size(X, 2);
  mdl.PredictorNames = opts.PredictorNames;
  mdl.ConvergenceInfo = struct('Converged', smo.Converged, 'Iterations', smo.Iterations);
end
%=============================================================================
function opts = parseOptionsLocal(args, X, classNames)
  opts = struct('ClassNames', {classNames}, 'KernelFunction', 'linear', ...
    'KernelScale', kernelScaleAutoLocal(X), 'PolynomialOrder', 3, 'BoxConstraint', 1, ...
    'BoxConstraints', [], 'Cost', [], 'Standardize', false, 'IterationLimit', 1000, 'Tolerance', 1e-3, ...
    'PredictorNames', {__default_predictor_names__(size(X, 2))});
  for k = 1:2:length(args)
    name = __option_name__(args{k}, {'classnames', 'kernelfunction', 'kernelscale', ...
      'polynomialorder', 'boxconstraint', 'standardize', 'iterationlimit', ...
      'tolerance', 'predictornames', 'cost'});
    value = args{k + 1};
    switch name
      case 'classnames'
        opts.ClassNames = __class_names__(value, classNames);
      case 'kernelfunction'
        opts.KernelFunction = kernelFunctionLocal(value);
      case 'kernelscale'
        opts.KernelScale = kernelScaleLocal(value, X);
      case 'polynomialorder'
        opts.PolynomialOrder = positiveIntegerLocal(value, 'PolynomialOrder');
      case 'boxconstraint'
        opts.BoxConstraint = positiveScalarLocal(value, 'BoxConstraint');
      case 'standardize'
        opts.Standardize = logicalScalarLocal(value, 'Standardize');
      case 'iterationlimit'
        opts.IterationLimit = positiveIntegerLocal(value, 'IterationLimit');
      case 'tolerance'
        opts.Tolerance = positiveScalarLocal(value, 'Tolerance');
      case 'predictornames'
        opts.PredictorNames = __predictor_names__(value, size(X, 2));
      case 'cost'
        opts.Cost = costMatrixLocal(value, length(classNames));
    end
  end
  if ~isequal(opts.ClassNames, classNames)
    __check_class_names__(classNames, opts.ClassNames, true);
  end
end
%=============================================================================
function smo = trainSmoLocal(K, y, opts)
  n = length(y);
  alpha = zeros(n, 1);
  b = 0;
  passes = 0;
  iter = 0;
  while passes < 5 && iter < opts.IterationLimit
    changed = 0;
    for i = 1:n
      decision = K * (alpha .* y) + b;
      E = decision - y;
      if ~violatesKktLocal(y(i), E(i), alpha(i), opts, i)
        continue
      end
      [~, j] = max(abs(E - E(i)));
      if j == i
        j = mod(i, n) + 1;
      end
      [alpha, b, didChange] = updatePairLocal(K, y, alpha, b, E, i, j, opts);
      changed = changed + didChange;
    end
    if changed == 0
      passes = passes + 1;
    else
      passes = 0;
    end
    iter = iter + 1;
  end
  smo = struct('Alpha', alpha, 'Bias', b, 'Converged', passes >= 5, 'Iterations', iter);
end
%=============================================================================
function tf = violatesKktLocal(y, E, alpha, opts, i)
  r = y * E;
  tf = (r < -opts.Tolerance && alpha < opts.BoxConstraints(i)) ...
    || (r > opts.Tolerance && alpha > 0);
end
%=============================================================================
function [alpha, b, changed] = updatePairLocal(K, y, alpha, b, E, i, j, opts)
  changed = 0;
  ai = alpha(i);
  aj = alpha(j);
  ci = opts.BoxConstraints(i);
  cj = opts.BoxConstraints(j);
  if y(i) ~= y(j)
    L = max(0, aj - ai);
    H = min(cj, ci + aj - ai);
  else
    L = max(0, ai + aj - ci);
    H = min(cj, ai + aj);
  end
  if L == H
    return
  end
  eta = 2 * K(i, j) - K(i, i) - K(j, j);
  if eta >= 0
    return
  end
  alpha(j) = min(H, max(L, aj - y(j) * (E(i) - E(j)) / eta));
  if abs(alpha(j) - aj) < 1e-12
    alpha(j) = aj;
    return
  end
  alpha(i) = ai + y(i) * y(j) * (aj - alpha(j));
  b1 = b - E(i) - y(i) * (alpha(i) - ai) * K(i, i) - y(j) * (alpha(j) - aj) * K(i, j);
  b2 = b - E(j) - y(i) * (alpha(i) - ai) * K(i, j) - y(j) * (alpha(j) - aj) * K(j, j);
  if alpha(i) > 0 && alpha(i) < ci
    b = b1;
  elseif alpha(j) > 0 && alpha(j) < cj
    b = b2;
  else
    b = (b1 + b2) / 2;
  end
  changed = 1;
end
%=============================================================================
function boxConstraints = costConstraintsLocal(yIndex, opts)
  boxConstraints = opts.BoxConstraint * ones(length(yIndex), 1);
  if isempty(opts.Cost)
    return
  end
  k = size(opts.Cost, 1);
  for row = 1:length(yIndex)
    current = yIndex(row);
    others = 1:k;
    others(current) = [];
    penalty = max(opts.Cost(current, others));
    boxConstraints(row) = opts.BoxConstraint * max(penalty, eps);
  end
end
%=============================================================================
function cost = costMatrixLocal(value, k)
  if ~(isnumeric(value) && isreal(value) && isequal(size(value), [k k]) ...
      && all(isfinite(value(:))) && all(value(:) >= 0))
    error('Nelson:statistics:costNonnegativeSquareMatrixOne', 'Cost must be a nonnegative square matrix with one row and column per class.');
  end
  cost = double(value);
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
