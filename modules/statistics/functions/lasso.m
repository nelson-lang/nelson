%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, FitInfo] = lasso(X, y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 2);
  opts = parseOptionsLocal(size(X, 1), convertStringToCharArgs(varargin));
  [X, y, opts] = validateInputsLocal(X, y, opts);
  keep = ~isnan(y) & all(~isnan(X), 2) & ~isnan(opts.Weights);
  [B, FitInfo] = lassoFitLocal(X(keep, :), y(keep), opts, opts.Weights(keep));
end
%=============================================================================
function opts = parseOptionsLocal(n, args)
  opts = struct('Alpha', 1, 'Lambda', [], 'LambdaRatio', 1e-4, ...
    'NumLambda', 100, 'Standardize', true, 'Intercept', true, ...
    'MaxIter', 1000, 'RelTol', 1e-4, 'Weights', ones(n, 1));
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{k}));
    switch name
      case 'alpha'
        opts.Alpha = parsePositiveScalarLocal(args{k + 1}, 'ALPHA');
        if opts.Alpha > 1
          error('Nelson:statistics:ALPHARange01', 'ALPHA must be in the range (0,1].');
        end
      case 'lambda'
        opts.Lambda = parseLambdaLocal(args{k + 1});
      case 'lambdaratio'
        opts.LambdaRatio = parseLambdaRatioLocal(args{k + 1});
      case 'numlambda'
        opts.NumLambda = parsePositiveIntegerLocal(args{k + 1}, 'NUMLAMBDA');
      case 'standardize'
        opts.Standardize = parseLogicalScalarLocal(args{k + 1}, 'STANDARDIZE');
      case 'intercept'
        opts.Intercept = parseLogicalScalarLocal(args{k + 1}, 'INTERCEPT');
      case 'maxiter'
        opts.MaxIter = parsePositiveIntegerLocal(args{k + 1}, 'MAXITER');
      case 'reltol'
        opts.RelTol = parsePositiveScalarLocal(args{k + 1}, 'RELTOL');
      case 'weights'
        opts.Weights = parseWeightsLocal(args{k + 1}, n);
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
  if ~opts.Intercept
    opts.Standardize = false;
  end
end
%=============================================================================
function [X, y, opts] = validateInputsLocal(X, y, opts)
  if ~(isnumeric(X) && isreal(X) && ismatrix(X) && ~issparse(X))
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if ~(isnumeric(y) && isreal(y) && isvector(y) && ~issparse(y))
    error('Nelson:statistics:yRealNumericVector', '%s must be a real numeric vector.', 'Y');
  end
  X = double(X);
  y = double(y(:));
  if size(X, 1) ~= length(y)
    error('Nelson:statistics:xYSameNumberRows', '%s must have the same number of rows.', 'X and Y');
  end
  opts.Weights = double(opts.Weights(:));
end
%=============================================================================
function [B, FitInfo] = lassoFitLocal(X, y, opts, weights)
  n = length(y);
  p = size(X, 2);
  if n == 0 || p == 0
    lambda = lambdaValuesLocal(zeros(0, p), zeros(0, 1), opts);
    B = NaN(p, length(lambda));
    FitInfo = fitInfoLocal(B, y, X, lambda, zeros(length(lambda), 1), opts, zeros(1, length(lambda)));
    return
  end
  weights = weights * n / sum(weights);
  [Xs, yc, xmean, xscale, ymean] = prepareDataLocal(X, y, weights, opts);
  lambda = lambdaValuesLocal(Xs, yc, opts, weights);
  Bwork = zeros(p, length(lambda));
  intercept = zeros(1, length(lambda));
  iterations = zeros(1, length(lambda));
  beta = zeros(p, 1);
  for j = 1:length(lambda)
    [beta, iterations(j)] = coordinateDescentLocal(Xs, yc, weights, lambda(j), opts, beta);
    Bwork(:, j) = beta;
    [Bwork(:, j), intercept(j)] = restoreScaleLocal(beta, xmean, xscale, ymean, opts);
  end
  B = Bwork;
  FitInfo = fitInfoLocal(B, y, X, lambda, intercept, opts, iterations);
end
%=============================================================================
function [Xs, yc, xmean, xscale, ymean] = prepareDataLocal(X, y, weights, opts)
  if opts.Intercept
    xmean = sum(X .* weights, 1) / sum(weights);
    ymean = sum(y .* weights) / sum(weights);
  else
    xmean = zeros(1, size(X, 2));
    ymean = 0;
  end
  Xs = X - xmean;
  yc = y - ymean;
  if opts.Standardize
    xscale = sqrt(sum((Xs .^ 2) .* weights, 1) / max(sum(weights) - 1, 1));
    xscale(xscale == 0 | isnan(xscale)) = 1;
    Xs = Xs ./ xscale;
  else
    xscale = ones(1, size(X, 2));
  end
end
%=============================================================================
function lambda = lambdaValuesLocal(X, y, opts, weights)
  if ~isempty(opts.Lambda)
    lambda = opts.Lambda;
    return
  end
  if isempty(y)
    lambda = NaN(1, opts.NumLambda);
    return
  end
  n = length(y);
  lambdaMax = max(abs(X' * (weights .* y))) / (n * opts.Alpha);
  if lambdaMax == 0 || isnan(lambdaMax)
    lambdaMax = 1;
  end
  if opts.LambdaRatio == 0
    if opts.NumLambda == 1
      lambda = 0;
    else
      lambda = [logspace(log10(lambdaMax), log10(lambdaMax * 1e-4), opts.NumLambda - 1), 0];
    end
  else
    lambda = logspace(log10(lambdaMax), log10(lambdaMax * opts.LambdaRatio), opts.NumLambda);
  end
end
%=============================================================================
function [beta, iter] = coordinateDescentLocal(X, y, weights, lambda, opts, beta)
  n = length(y);
  xnorm = sum((X .^ 2) .* weights, 1)' / n;
  xnorm(xnorm == 0) = 1;
  residual = y - X * beta;
  for iter = 1:opts.MaxIter
    old = beta;
    for j = 1:length(beta)
      residual = residual + X(:, j) * beta(j);
      rho = sum(weights .* X(:, j) .* residual) / n;
      beta(j) = softThresholdLocal(rho, lambda * opts.Alpha) ...
        / (xnorm(j) + lambda * (1 - opts.Alpha));
      residual = residual - X(:, j) * beta(j);
    end
    if norm(beta - old) <= opts.RelTol * max(norm(old), 1)
      return
    end
  end
end
%=============================================================================
function value = softThresholdLocal(value, threshold)
  value = sign(value) .* max(abs(value) - threshold, 0);
end
%=============================================================================
function [beta, intercept] = restoreScaleLocal(beta, xmean, xscale, ymean, opts)
  beta = beta ./ xscale(:);
  if opts.Intercept
    intercept = ymean - xmean * beta;
  else
    intercept = 0;
  end
end
%=============================================================================
function FitInfo = fitInfoLocal(B, y, X, lambda, intercept, opts, iterations)
  FitInfo = struct();
  FitInfo.Intercept = intercept;
  FitInfo.Lambda = lambda;
  FitInfo.Alpha = opts.Alpha;
  FitInfo.DF = sum(B ~= 0, 1);
  yhat = X * B + repmat(intercept, length(y), 1);
  FitInfo.MSE = mean((repmat(y, 1, size(B, 2)) - yhat) .^ 2, 1);
  FitInfo.NumIterations = iterations;
end
%=============================================================================
function value = parseLambdaLocal(value)
  if ~(isnumeric(value) && isreal(value) && (isvector(value) || isempty(value)) ...
      && all(isfinite(value)) && all(value >= 0))
    error('Nelson:statistics:LAMBDANonnegativeNumericVector', '%s must be a nonnegative numeric vector.', 'LAMBDA');
  end
  value = double(value(:).');
end
%=============================================================================
function value = parseLambdaRatioLocal(value)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value >= 0 && value < 1)
    error('Nelson:statistics:LAMBDARATIORange01', 'LAMBDARATIO must be in the range [0,1).');
  end
  value = double(value);
end
%=============================================================================
function value = parsePositiveScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0)
    error('Nelson:statistics:mustBePositive', '%s must be a positive scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = parsePositiveIntegerLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value == fix(value) && value > 0)
    error('Nelson:statistics:mustBePositiveInteger', '%s must be a positive integer scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = parseLogicalScalarLocal(value, name)
  if ~((isnumeric(value) || islogical(value)) && isreal(value) && isscalar(value) ...
      && (value == 0 || value == 1))
    error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', name);
  end
  value = logical(value);
end
%=============================================================================
function weights = parseWeightsLocal(value, n)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && length(value) == n)
    error('Nelson:statistics:weightsNumericVectorOneValue', '%s must be a numeric vector with one value per observation.', 'WEIGHTS');
  end
  weights = double(value(:));
  if any(isnan(weights)) || any(weights < 0) || all(weights == 0)
    error('Nelson:statistics:WEIGHTSContainNonnegativeValuesLeast', 'WEIGHTS must contain nonnegative values with at least one positive value.');
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
