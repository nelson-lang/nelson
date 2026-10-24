%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [coeff, score, pcvar, mu, v, S] = ppca(Y, K, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 6);
  preserveSingle = isa(Y, 'single');
  [Y, K, opts] = parseInputsLocal(Y, K, convertStringToCharArgs(varargin));
  [coeff, score, pcvar, mu, v, S] = fitLocal(Y, K, opts);
  if preserveSingle
    coeff = single(coeff);
    score = single(score);
    pcvar = single(pcvar);
    mu = single(mu);
    v = single(v);
    S.W = single(S.W);
    S.Xexp = single(S.Xexp);
    S.Recon = single(S.Recon);
    S.v = single(S.v);
    S.RMSResid = single(S.RMSResid);
    S.nloglk = single(S.nloglk);
  end
end
%=============================================================================
function [Y, K, opts] = parseInputsLocal(Y, K, args)
  if ~(isnumeric(Y) && isreal(Y) && ~issparse(Y) && ndims(Y) == 2)
    error('Nelson:statistics:yRealNumericMatrix', '%s must be a real numeric matrix.', 'Y');
  end
  Y = double(Y);
  if isempty(Y) || size(Y, 1) < 2 || size(Y, 2) < 2
    error('Nelson:statistics:yContainLeastTwoObservations', 'Y must contain at least two observations and two variables.');
  end
  if any(isinf(Y(:)))
    error('Nelson:statistics:yContainInfValues', '%s must not contain Inf values.', 'Y');
  end
  maxK = min(size(Y, 1), size(Y, 2)) - 1;
  if ~(isnumeric(K) && isreal(K) && isscalar(K) && isfinite(K) ...
      && floor(K) == K && K > 0)
    error('Nelson:statistics:kPositiveIntegerScalar', '%s must be a positive integer scalar.', 'K');
  end
  K = min(double(K), maxK);
  if K < 1
    error('Nelson:statistics:kSmallerMaximumPossibleRank', 'K must be smaller than the maximum possible rank.');
  end
  opts = defaultOptionsLocal();
  opts = parseNameValuesLocal(opts, args);
  opts.W0 = validateW0Local(opts.W0, size(Y, 2), K);
  opts.v0 = validateV0Local(opts.v0);
  opts.Options = parseOptionsLocal(opts.Options);
end
%=============================================================================
function opts = defaultOptionsLocal()
  opts = struct('W0', [], 'v0', [], 'Options', []);
end
%=============================================================================
function opts = parseNameValuesLocal(opts, args)
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{k}));
    switch name
      case 'w0'
        opts.W0 = args{k + 1};
      case 'v0'
        opts.v0 = args{k + 1};
      case 'options'
        opts.Options = args{k + 1};
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
  end
end
%=============================================================================
function W0 = validateW0Local(W0, nvars, K)
  if isempty(W0)
    return
  end
  if ~(isnumeric(W0) && isreal(W0) && ~issparse(W0) && ndims(W0) == 2)
    error('Nelson:statistics:W0RealNumericMatrix', '%s must be a real numeric matrix.', 'W0');
  end
  W0 = double(W0);
  if size(W0, 1) ~= nvars || size(W0, 2) < K
    error('Nelson:statistics:W0IncompatibleSize', '%s has incompatible size.', 'W0');
  end
  if any(~isfinite(W0(:)))
    error('Nelson:statistics:W0ContainFiniteValues', '%s must contain finite values.', 'W0');
  end
  W0 = W0(:, 1:K);
end
%=============================================================================
function v0 = validateV0Local(v0)
  if isempty(v0)
    return
  end
  if ~(isnumeric(v0) && isreal(v0) && isscalar(v0) && isfinite(v0) && v0 > 0)
    error('Nelson:statistics:v0PositiveScalar', '%s must be a positive scalar.', 'v0');
  end
  v0 = double(v0);
end
%=============================================================================
function options = parseOptionsLocal(options)
  if isempty(options)
    options = statset();
  elseif ~(isstruct(options) && isscalar(options))
    error('Nelson:statistics:invalidOptions', '%s must be a scalar structure.', 'Options');
  end
  displayValue = statget(options, 'Display', 'off');
  if ~isTextScalarLocal(displayValue)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Options Display');
  end
  options.Display = lower(char(displayValue));
  options.MaxIter = statget(options, 'MaxIter', 1000);
  options.TolFun = statget(options, 'TolFun', 1e-6);
  options.TolX = statget(options, 'TolX', 1e-6);
  if ~any(strcmp(options.Display, {'off', 'final', 'iter'}))
    error('Nelson:statistics:optionsDisplayInvalidValue', '%s has an invalid value.', 'Options Display');
  end
  if ~isPositiveIntegerLocal(options.MaxIter)
    error('Nelson:statistics:optionsMaxIterPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Options MaxIter');
  end
  if ~isPositiveScalarLocal(options.TolFun) || ~isPositiveScalarLocal(options.TolX)
    error('Nelson:statistics:optionsTolerancesPositiveScalarValues', 'Options tolerances must be %s scalar values.', 'positive');
  end
end
%=============================================================================
function [coeff, score, pcvar, mu, v, S] = fitLocal(Y, K, opts)
  missing = isnan(Y);
  mu = meanOmitNaNLocal(Y);
  Xfilled = initialFilledLocal(Y, mu, missing, opts.W0);
  previousCost = Inf;
  step = Inf;
  for iter = 1:opts.Options.MaxIter
    [coeff, score, pcvar] = pcaComponentsLocal(Xfilled, K);
    recon = score * coeff' + repmat(mean(Xfilled, 1), size(Y, 1), 1);
    Xnew = Xfilled;
    if any(missing(:))
      Xnew(missing) = recon(missing);
    end
    cost = observedRMSLocal(Y, recon, missing);
    step = max(abs(Xnew(:) - Xfilled(:))) / max(1, max(abs(Xfilled(:))));
    displayProgressLocal(opts.Options.Display, 'iter', iter, cost, step);
    Xfilled = Xnew;
    if abs(previousCost - cost) <= opts.Options.TolFun * max(1, previousCost) ...
        || step <= opts.Options.TolX
      previousCost = cost;
      break
    end
    previousCost = cost;
  end
  [coeff, score, pcvar, mu] = finalComponentsLocal(Xfilled, Y, K);
  recon = score * coeff' + repmat(mu, size(Y, 1), 1);
  v = residualVarianceLocal(Y, recon, missing, opts.v0);
  S = convergenceStructLocal(coeff, score, pcvar, recon, v, previousCost, iter);
  displayProgressLocal(opts.Options.Display, 'final', iter, previousCost, step);
end
%=============================================================================
function mu = meanOmitNaNLocal(Y)
  mu = zeros(1, size(Y, 2));
  for j = 1:size(Y, 2)
    values = Y(~isnan(Y(:, j)), j);
    if isempty(values)
      error('Nelson:statistics:variableContainLeastOneObserved', 'Each variable must contain at least one observed value.');
    end
    mu(j) = mean(values);
  end
end
%=============================================================================
function Xfilled = initialFilledLocal(Y, mu, missing, W0)
  Xfilled = Y;
  if ~any(missing(:))
    return
  end
  Xfilled(missing) = 0;
  for j = 1:size(Y, 2)
    if any(missing(:, j))
      Xfilled(missing(:, j), j) = mu(j);
    end
  end
  if ~isempty(W0)
    [U, ~, ~] = svd(W0, 'econ');
    coeff = U(:, 1:size(W0, 2));
    score = (Xfilled - repmat(mu, size(Y, 1), 1)) * coeff;
    startRecon = score * coeff' + repmat(mu, size(Y, 1), 1);
    if any(missing(:))
      Xfilled(missing) = startRecon(missing);
    end
  end
end
%=============================================================================
function [coeff, score, pcvar, mu] = finalComponentsLocal(Xfilled, Y, K)
  mu = mean(Xfilled, 1);
  mu = mu(:)';
  [coeff, score, pcvar] = pcaComponentsLocal(Xfilled, K);
end
%=============================================================================
function [coeff, score, pcvar] = pcaComponentsLocal(X, K)
  [coeffAll, ~, pcvarAll] = pca(X);
  nvars = size(X, 2);
  available = min(K, size(coeffAll, 2));
  coeff = zeros(nvars, K);
  pcvar = zeros(K, 1);
  if available > 0
    coeff(:, 1:available) = coeffAll(:, 1:available);
    pcvar(1:available) = pcvarAll(1:available);
  end
  if available < K
    coeff = completeBasisLocal(coeff, available);
  end
  score = (X - repmat(mean(X, 1), size(X, 1), 1)) * coeff;
end
%=============================================================================
function coeff = completeBasisLocal(coeff, available)
  nvars = size(coeff, 1);
  for col = available + 1:size(coeff, 2)
    v = zeros(nvars, 1);
    v(min(col, nvars)) = 1;
    for previous = 1:col - 1
      v = v - coeff(:, previous) * (coeff(:, previous)' * v);
    end
    vnorm = norm(v);
    if vnorm <= eps
      v = zeros(nvars, 1);
      v(1) = 1;
      vnorm = 1;
    end
    coeff(:, col) = v / vnorm;
  end
end
%=============================================================================
function rmsValue = observedRMSLocal(Y, recon, missing)
  observed = ~missing;
  if any(observed(:))
    residual = Y(observed) - recon(observed);
    rmsValue = sqrt(mean(residual .^ 2));
  else
    rmsValue = NaN;
  end
end
%=============================================================================
function v = residualVarianceLocal(Y, recon, missing, v0)
  observed = ~missing;
  residual = Y(observed) - recon(observed);
  if isempty(residual)
    v = v0;
  else
    v = mean(residual .^ 2);
    if ~isempty(v0)
      v = max(v, v0 * eps);
    end
  end
  v = max(v, eps);
end
%=============================================================================
function S = convergenceStructLocal(coeff, score, pcvar, recon, v, rmsResid, iter)
  W = coeff * diag(sqrt(max(pcvar(:) - v, 0)));
  nloglk = 0.5 * numel(recon) * (log(2 * pi * max(v, realmin)) + 1);
  S = struct('W', W, 'Xexp', score, 'Recon', recon, 'v', v, ...
    'NumIter', iter, 'RMSResid', rmsResid, 'nloglk', nloglk);
end
%=============================================================================
function displayProgressLocal(displayValue, stage, iter, cost, step)
  if strcmp(displayValue, 'iter') && strcmp(stage, 'iter')
    disp([int2str(iter), ' iterations, rms resid = ', num2str(cost), ...
      ' |delta x| = ', num2str(step)]);
  elseif strcmp(displayValue, 'final') && strcmp(stage, 'final')
    disp([int2str(iter), ' iterations, Final rms residual = ', num2str(cost)]);
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
