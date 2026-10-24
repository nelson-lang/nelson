%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [lambda, psi, T, stats, F] = factoran(X, m, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 5);
  [X, m, opts] = parseInputsLocal(X, m, convertStringToCharArgs(varargin));
  [S, Z, nobs] = covarianceInputLocal(X, opts.Xtype);
  [lambda, psi] = loadingsLocal(S, m);
  if strcmp(opts.Rotate, 'none')
    T = eye(m);
  else
    [lambda, T] = rotateLoadingsLocal(lambda, opts);
  end
  stats = statsLocal(S, lambda, psi, nobs);
  F = scoresLocal(Z, lambda, psi, opts.Scores);
end
%=============================================================================
function [X, m, opts] = parseInputsLocal(X, m, args)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  X = double(X);
  if isempty(X) || size(X, 2) < 2
    error('Nelson:statistics:xContainLeastTwoVariables', 'X must contain at least two variables.');
  end
  if ~(isnumeric(m) && isreal(m) && isscalar(m) && isfinite(m) && floor(m) == m && m > 0)
    error('Nelson:statistics:mPositiveIntegerScalar', '%s must be a positive integer scalar.', 'M');
  end
  m = double(m);
  opts = defaultOptionsLocal();
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    opts = setOptionLocal(opts, lower(char(args{k})), args{k + 1});
  end
end
%=============================================================================
function opts = defaultOptionsLocal()
  opts = struct('Xtype', 'data', 'Rotate', 'varimax', 'Scores', 'wls', ...
    'MaxIt', 250, 'RelTol', sqrt(eps), 'RotateCoeff', 1, ...
    'Normalize', true, 'Power', 4, 'Target', [], 'Type', 'oblique');
end
%=============================================================================
function opts = setOptionLocal(opts, name, value)
  switch name
    case 'xtype'
      opts.Xtype = parseChoiceLocal(value, {'data', 'covariance', 'correlation'}, 'Xtype');
      if strcmp(opts.Xtype, 'correlation')
        opts.Xtype = 'covariance';
      end
    case 'rotate'
      opts.Rotate = parseChoiceLocal(value, ...
        {'varimax', 'none', 'quartimax', 'equamax', 'parsimax', 'orthomax', ...
         'promax', 'procrustes', 'pattern'}, 'Rotate');
    case 'scores'
      opts.Scores = parseChoiceLocal(value, {'wls', 'bartlett', 'regression', 'thomson', 'none'}, 'Scores');
      if strcmp(opts.Scores, 'bartlett')
        opts.Scores = 'wls';
      elseif strcmp(opts.Scores, 'thomson')
        opts.Scores = 'regression';
      end
    case 'options'
      opts = parseOptionsLocal(opts, value);
    case 'start'
      validateStartLocal(value);
    case 'coeff'
      opts.RotateCoeff = validateCoeffLocal(value);
    case 'normalize'
      opts.Normalize = parseOnOffLocal(value, 'Normalize');
    case 'reltol'
      opts.RelTol = validatePositiveScalarLocal(value, 'RelTol');
    case 'maxit'
      opts.MaxIt = validatePositiveIntegerLocal(value, 'MaxIt');
    case 'power'
      opts.Power = validatePositiveScalarLocal(value, 'Power');
    case 'target'
      opts.Target = validateTargetLocal(value, [0 0]);
    case 'type'
      opts.Type = parseChoiceLocal(value, {'oblique', 'orthogonal'}, 'Type');
    otherwise
      error('Nelson:statistics:unknownOption', 'Unknown option.');
  end
end
%=============================================================================
function value = parseChoiceLocal(value, choices, name)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
  value = lower(char(value));
  matches = find(startsWith(choices, value));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
  end
  value = choices{matches};
end
%=============================================================================
function opts = parseOptionsLocal(opts, options)
  if isempty(options)
    return
  end
  if ~(isstruct(options) && isscalar(options))
    error('Nelson:statistics:invalidOptions', '%s must be a scalar structure.', 'Options');
  end
  opts.MaxIt = statget(options, 'MaxIter', opts.MaxIt);
  opts.RelTol = statget(options, 'TolX', opts.RelTol);
  if ~isPositiveIntegerLocal(opts.MaxIt)
    error('Nelson:statistics:optionsMaxIterPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Options MaxIter');
  end
  if ~isPositiveScalarLocal(opts.RelTol)
    error('Nelson:statistics:optionsTolXPositiveScalar', '%s must be a positive scalar.', 'Options TolX');
  end
end
%=============================================================================
function validateStartLocal(value)
  if isTextScalarLocal(value)
    parseChoiceLocal(value, {'rsquared', 'random'}, 'Start');
    return
  end
  if ~(isnumeric(value) && isreal(value) && ~issparse(value) && all(isfinite(double(value(:)))))
    error('Nelson:statistics:startSupportedValueFiniteNumeric', 'Start must be a supported value or a finite numeric array.');
  end
  if isscalar(value) && (value <= 0 || floor(value) ~= value)
    error('Nelson:statistics:startPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Start');
  end
end
%=============================================================================
function value = validateCoeffLocal(value)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value >= 0)
    error('Nelson:statistics:coeffNonnegativeFiniteScalar', 'Coeff must be a nonnegative finite scalar.');
  end
  value = double(value);
end
%=============================================================================
function value = validatePositiveScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0)
    error('Nelson:statistics:mustBePositiveFinite', '%s must be a positive finite scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = validatePositiveIntegerLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && floor(value) == value && value > 0)
    error('Nelson:statistics:mustBePositiveInteger', '%s must be a positive integer scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function target = validateTargetLocal(target, expectedSize)
  if ~(isnumeric(target) && isreal(target) && ~issparse(target) && ndims(target) == 2)
    error('Nelson:statistics:targetRealNumericMatrix', '%s must be a real numeric matrix.', 'Target');
  end
  target = double(target);
  if ~isequal(expectedSize, [0 0]) && ~isequal(size(target), expectedSize)
    error('Nelson:statistics:targetSameSizeLoadings', 'Target must have the same size as loadings.');
  end
  if any(~isfinite(target(:)))
    error('Nelson:statistics:targetContainFiniteValues', '%s must contain finite values.', 'Target');
  end
end
%=============================================================================
function tf = parseOnOffLocal(value, name)
  if islogical(value) && isscalar(value)
    tf = value ~= 0;
  elseif isnumeric(value) && isreal(value) && isscalar(value) && (value == 0 || value == 1)
    tf = value ~= 0;
  elseif isTextScalarLocal(value)
    value = lower(char(value));
    if strcmp(value, 'on')
      tf = true;
    elseif strcmp(value, 'off')
      tf = false;
    else
      error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
    end
  else
    error('Nelson:statistics:invalidValue:valueOffTrueFalse', '%s must be on, off, true, or false.', name);
  end
end
%=============================================================================
function [S, Z, nobs] = covarianceInputLocal(X, xtype)
  if strcmp(xtype, 'covariance')
    S = validateCovarianceLocal(X);
    Z = [];
    nobs = NaN;
    return
  end
  if any(isinf(X(:)))
    error('Nelson:statistics:xMustBeFinite', '%s must contain finite values.', 'X');
  end
  keep = ~any(isnan(X), 2);
  X = X(keep, :);
  if size(X, 1) < 2
    error('Nelson:statistics:xContainLeastTwoComplete', 'X must contain at least two complete observations.');
  end
  mu = mean(X, 1);
  X = X - repmat(mu, size(X, 1), 1);
  scale = sqrt(sum(X .^ 2, 1) / max(size(X, 1) - 1, 1));
  if any(scale <= 0)
    error('Nelson:statistics:variablePositiveVariance', 'Each variable must have positive variance.');
  end
  Z = X ./ repmat(scale, size(X, 1), 1);
  S = (Z' * Z) / max(size(Z, 1) - 1, 1);
  S = (S + S') / 2;
  nobs = size(Z, 1);
end
%=============================================================================
function S = validateCovarianceLocal(S)
  if size(S, 1) ~= size(S, 2)
    error('Nelson:statistics:xSquareMatrixXtypeCovariance', 'X must be a square matrix when Xtype is covariance.');
  end
  if any(~isfinite(S(:)))
    error('Nelson:statistics:xMustBeFinite', '%s must contain finite values.', 'X');
  end
  tolerance = 100 * eps(max(1, max(abs(S(:)))));
  if any(any(abs(S - S') > tolerance))
    error('Nelson:statistics:xSymmetricXtypeCovariance', 'X must be symmetric when Xtype is covariance.');
  end
  S = (S + S') / 2;
  if any(diag(S) <= 0)
    error('Nelson:statistics:xPositiveDiagonal', 'X must have a positive diagonal.');
  end
end
%=============================================================================
function [lambda, psi] = loadingsLocal(S, m)
  p = size(S, 1);
  if m >= p
    error('Nelson:statistics:mSmallerNumberVariables', 'M must be smaller than the number of variables.');
  end
  [V, D] = eig(S);
  latent = real(diag(D));
  [latent, order] = sort(latent, 'descend');
  V = real(V(:, order));
  tolerance = 100 * eps(max(1, max(abs(latent))));
  if any(latent <= tolerance)
    error('Nelson:statistics:xPositiveDefinite', 'X must be positive definite.');
  end
  lambda = V(:, 1:m) * diag(sqrt(max(latent(1:m), 0)));
  lambda = orientColumnsLocal(lambda);
  psi = diag(S) - (lambda .^ 2) * ones(m, 1);
  minPsi = eps(max(diag(S)));
  psi(psi < minPsi) = minPsi;
end
%=============================================================================
function lambda = orientColumnsLocal(lambda)
  for k = 1:size(lambda, 2)
    [~, idx] = max(abs(lambda(:, k)));
    if lambda(idx, k) < 0
      lambda(:, k) = -lambda(:, k);
    end
  end
end
%=============================================================================
function [lambda, T] = rotateLoadingsLocal(lambda, opts)
  args = {'Method', opts.Rotate, 'Normalize', opts.Normalize, 'RelTol', opts.RelTol, ...
    'MaxIt', opts.MaxIt, 'Coeff', opts.RotateCoeff, 'Power', opts.Power, 'Type', opts.Type};
  if ~isempty(opts.Target)
    if ~isequal(size(opts.Target), size(lambda))
      error('Nelson:statistics:targetSameSizeLoadings', 'Target must have the same size as loadings.');
    end
    args = [args, {'Target', opts.Target}];
  end
  [lambda, T] = rotatefactors(lambda, args{:});
end
%=============================================================================
function stats = statsLocal(S, lambda, psi, nobs)
  sigma = lambda * lambda' + diag(psi);
  residual = S - sigma;
  residual(1:size(residual, 1) + 1:end) = 0;
  p = size(S, 1);
  m = size(lambda, 2);
  dfe = ((p - m) ^ 2 - p - m) / 2;
  objective = likelihoodObjectiveLocal(S, sigma);
  chisq = NaN;
  prob = NaN;
  if isfinite(nobs) && dfe > 0 && isfinite(objective)
    scale = max(nobs - 1 - (2 * p + 5) / 6 - (2 * m) / 3, 1);
    chisq = scale * objective;
    prob = 1 - chi2cdf(chisq, dfe);
  end
  stats = struct('loglike', -0.5 * max(nobs, 1) * objective, ...
    'dfe', dfe, 'chisq', chisq, 'p', prob, 'residual', residual);
end
%=============================================================================
function objective = likelihoodObjectiveLocal(S, sigma)
  [signS, logS] = logdetLocal(S);
  [signSigma, logSigma] = logdetLocal(sigma);
  if signS <= 0 || signSigma <= 0
    objective = NaN;
  else
    objective = logSigma + trace(sigma \ S) - logS - size(S, 1);
  end
end
%=============================================================================
function [sgn, value] = logdetLocal(A)
  [~, U, P] = lu(A);
  sgn = det(P);
  diagonal = diag(U);
  sgn = sgn * prod(sign(diagonal));
  value = sum(log(abs(diagonal)));
end
%=============================================================================
function F = scoresLocal(Z, lambda, psi, method)
  if isempty(Z) || strcmp(method, 'none')
    F = [];
    return
  end
  psi = max(psi(:), eps);
  if strcmp(method, 'wls')
    weightedLambda = lambda ./ repmat(psi, 1, size(lambda, 2));
    B = (lambda' * weightedLambda) \ weightedLambda';
    F = Z * B';
  else
    sigma = lambda * lambda' + diag(psi);
    F = Z * (sigma \ lambda);
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
function tf = isPositiveIntegerLocal(value)
  tf = isnumeric(value) && isreal(value) && isscalar(value) ...
    && isfinite(value) && floor(value) == value && value > 0;
end
%=============================================================================
function tf = isPositiveScalarLocal(value)
  tf = isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0;
end
%=============================================================================
