%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitglm(X, y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, y] = validateInputsLocal(X, y);
  [modelspec, args] = parseModelSpecLocal(varargin);
  opts = parseOptionsLocal(args);
  terms = modelTermsLocal(size(X, 2), modelspec);
  keep = ~isnan(y) & all(~isnan(X), 2);
  if ~any(keep)
    error('Nelson:statistics:validObservationsRemainAfterRemoving', 'No valid observations remain after removing missing values.');
  end
  fit = fitGlmLocal(X(keep, :), y(keep), terms, opts);
  mdl = buildModelLocal(fit, X, y, keep, terms, opts);
end
%=============================================================================
function [X, y] = validateInputsLocal(X, y)
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
  if size(X, 2) == 0
    error('Nelson:statistics:xContainLeastOnePredictor', 'X must contain at least one predictor.');
  end
end
%=============================================================================
function [modelspec, args] = parseModelSpecLocal(args)
  modelspec = 'linear';
  if ~isempty(args) && isModelSpecValueLocal(args{1})
    modelspec = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function tf = isModelSpecValueLocal(value)
  tf = isnumeric(value) && ~isscalar(value);
  if ischar(value) || (isstring(value) && isscalar(value))
    specs = {'constant', 'linear', 'additive', 'interactions', 'interaction', ...
      'quadratic', 'purequadratic'};
    tf = any(strcmp(lower(char(value)), specs));
  end
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Intercept', true, 'PredictorNames', [], 'ResponseName', 'y', ...
    'Distribution', 'normal', 'Link', '', 'MaxIter', 100, 'TolFun', 1e-8);
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(args)
    name = optionNameLocal(args{k});
    value = args{k + 1};
    switch name
      case 'intercept'
        opts.Intercept = logicalScalarLocal(value, 'Intercept');
      case 'predictornames'
        opts.PredictorNames = textVectorLocal(value, 'PredictorNames');
      case 'responsename'
        opts.ResponseName = responseNameLocal(value);
      case 'distribution'
        opts.Distribution = distributionLocal(value);
      case 'link'
        opts.Link = linkLocal(value);
      case 'maxiter'
        opts.MaxIter = positiveIntegerLocal(value, 'MaxIter');
      case 'tolfun'
        opts.TolFun = positiveScalarLocal(value, 'TolFun');
    end
  end
  if isempty(opts.Link)
    opts.Link = canonicalLinkLocal(opts.Distribution);
  end
  validateDistributionLinkLocal(opts.Distribution, opts.Link);
end
%=============================================================================
function fit = fitGlmLocal(X, y, terms, opts)
  validateResponseLocal(y, opts.Distribution);
  D = designMatrixLocal(X, terms, opts.Intercept);
  if strcmp(opts.Distribution, 'normal') && strcmp(opts.Link, 'identity')
    fit = fitNormalLocal(D, y);
    fit.NumIterations = 1;
    fit.Converged = true;
  else
    fit = fitIrlsLocal(D, y, opts);
  end
  fit.X = X;
  fit.Terms = terms;
end
%=============================================================================
function fit = fitNormalLocal(D, y)
  beta = D \ y;
  eta = D * beta;
  mu = eta;
  residuals = y - mu;
  dfe = length(y) - rank(D);
  sse = sum(residuals .^ 2);
  dispersion = sse / max(dfe, 1);
  covb = dispersion * pinv(D' * D);
  fit = baseFitStructLocal(D, y, beta, eta, mu, residuals, covb, dfe, dispersion);
end
%=============================================================================
function fit = fitIrlsLocal(D, y, opts)
  beta = D \ initialEtaLocal(y, opts);
  converged = false;
  for iter = 1:opts.MaxIter
    eta = D * beta;
    mu = clippedMeanLocal(inverseLinkLocal(eta, opts.Link), opts.Distribution);
    dmu = derivativeMuEtaLocal(mu, opts.Link);
    variance = varianceLocal(mu, opts.Distribution);
    z = eta + (y - mu) ./ max(dmu, eps);
    w = (dmu .^ 2) ./ max(variance, eps);
    betaNew = weightedLeastSquaresLocal(D, z, w);
    if norm(betaNew - beta) <= opts.TolFun * max(1, norm(beta))
      beta = betaNew;
      converged = true;
      break
    end
    beta = betaNew;
  end
  eta = D * beta;
  mu = clippedMeanLocal(inverseLinkLocal(eta, opts.Link), opts.Distribution);
  residuals = y - mu;
  dfe = length(y) - rank(D);
  w = (derivativeMuEtaLocal(mu, opts.Link) .^ 2) ./ max(varianceLocal(mu, opts.Distribution), eps);
  covb = pinv(D' * (D .* repmat(w, 1, size(D, 2))));
  dispersion = dispersionLocal(y, mu, opts.Distribution, dfe);
  fit = baseFitStructLocal(D, y, beta, eta, mu, residuals, dispersion * covb, dfe, dispersion);
  fit.NumIterations = iter;
  fit.Converged = converged;
end
%=============================================================================
function fit = baseFitStructLocal(D, y, beta, eta, mu, residuals, covb, dfe, dispersion)
  fit = struct('D', D, 'Y', y, 'Beta', beta, 'LinearPredictor', eta, 'Fitted', mu, ...
    'Residuals', residuals, 'Covariance', covb, 'DFE', dfe, 'Dispersion', dispersion, ...
    'SSE', sum(residuals .^ 2), 'SST', sum((y - mean(y)) .^ 2), ...
    'NumObservations', length(y), 'NumCoefficients', length(beta), ...
    'NumIterations', 0, 'Converged', false);
end
%=============================================================================
function mdl = buildModelLocal(fit, X, y, keep, terms, opts)
  mdl = GeneralizedLinearModel();
  mdl.Intercept = opts.Intercept;
  mdl.Terms = terms;
  mdl.Beta = fit.Beta;
  mdl.Covariance = fit.Covariance;
  mdl.NumObservations = fit.NumObservations;
  mdl.NumPredictors = size(X, 2);
  mdl.DFE = fit.DFE;
  mdl.RMSE = sqrt(fit.SSE / max(fit.DFE, 1));
  mdl.SSE = fit.SSE;
  mdl.Deviance = devianceLocal(fit.Y, fit.Fitted, opts.Distribution);
  mdl.Dispersion = fit.Dispersion;
  mdl.Distribution = opts.Distribution;
  mdl.Link = opts.Link;
  mdl.Rsquared = rsquaredLocal(fit);
  mdl.NumIterations = fit.NumIterations;
  mdl.Converged = fit.Converged;
  mdl.PredictorNames = predictorNamesLocal(opts.PredictorNames, size(X, 2));
  mdl.ResponseName = opts.ResponseName;
  mdl.CoefficientNames = coefficientNamesLocal(terms, opts.Intercept, mdl.PredictorNames);
  mdl.Coefficients = coefficientsTableLocal(fit, mdl.CoefficientNames);
  mdl.Fitted = expandVectorLocal(fit.Fitted, keep, size(X, 1));
  raw = expandVectorLocal(fit.Residuals, keep, size(X, 1));
  mdl.Residuals = table(raw, 'VariableNames', {'Raw'});
  y;
end
%=============================================================================
function D = designMatrixLocal(X, terms, intercept)
  D = zeros(size(X, 1), 0);
  if intercept
    D = ones(size(X, 1), 1);
  end
  for k = 1:size(terms, 1)
    col = ones(size(X, 1), 1);
    for j = 1:size(X, 2)
      if terms(k, j) ~= 0
        col = col .* (X(:, j) .^ terms(k, j));
      end
    end
    D = [D col];
  end
end
%=============================================================================
function beta = weightedLeastSquaresLocal(D, z, w)
  sw = sqrt(max(w, eps));
  beta = (D .* repmat(sw, 1, size(D, 2))) \ (z .* sw);
end
%=============================================================================
function eta = initialEtaLocal(y, opts)
  switch opts.Distribution
    case 'binomial'
      mu = (y + 0.5) / 2;
    case 'poisson'
      mu = max(y + 0.1, 0.1);
    otherwise
      mu = y;
  end
  eta = linkValueLocal(mu, opts.Link);
end
%=============================================================================
function mu = inverseLinkLocal(eta, link)
  switch link
    case 'log'
      mu = exp(eta);
    case 'logit'
      mu = 1 ./ (1 + exp(-eta));
    otherwise
      mu = eta;
  end
end
%=============================================================================
function eta = linkValueLocal(mu, link)
  switch link
    case 'log'
      eta = log(max(mu, realmin));
    case 'logit'
      mu = min(max(mu, eps), 1 - eps);
      eta = log(mu ./ (1 - mu));
    otherwise
      eta = mu;
  end
end
%=============================================================================
function dmu = derivativeMuEtaLocal(mu, link)
  switch link
    case 'log'
      dmu = mu;
    case 'logit'
      dmu = mu .* (1 - mu);
    otherwise
      dmu = ones(size(mu));
  end
end
%=============================================================================
function variance = varianceLocal(mu, distribution)
  switch distribution
    case 'binomial'
      variance = mu .* (1 - mu);
    case 'poisson'
      variance = mu;
    otherwise
      variance = ones(size(mu));
  end
end
%=============================================================================
function mu = clippedMeanLocal(mu, distribution)
  switch distribution
    case 'binomial'
      mu = min(max(mu, eps), 1 - eps);
    case 'poisson'
      mu = max(mu, realmin);
  end
end
%=============================================================================
function dispersion = dispersionLocal(y, mu, distribution, dfe)
  if strcmp(distribution, 'normal')
    dispersion = sum((y - mu) .^ 2) / max(dfe, 1);
  else
    dispersion = 1;
  end
end
%=============================================================================
function value = devianceLocal(y, mu, distribution)
  switch distribution
    case 'binomial'
      mu = min(max(mu, eps), 1 - eps);
      term = zeros(size(y));
      pos = y > 0;
      neg = y < 1;
      term(pos) = term(pos) + y(pos) .* log(y(pos) ./ mu(pos));
      term(neg) = term(neg) + (1 - y(neg)) .* log((1 - y(neg)) ./ (1 - mu(neg)));
      value = 2 * sum(term);
    case 'poisson'
      term = mu;
      pos = y > 0;
      term(pos) = y(pos) .* log(y(pos) ./ mu(pos)) - (y(pos) - mu(pos));
      value = 2 * sum(term);
    otherwise
      value = sum((y - mu) .^ 2);
  end
end
%=============================================================================
function terms = modelTermsLocal(p, modelspec)
  if ischar(modelspec) || (isstring(modelspec) && isscalar(modelspec))
    switch lower(char(modelspec))
      case 'constant'
        terms = zeros(0, p);
      case {'linear', 'additive'}
        terms = eye(p);
      case {'interactions', 'interaction'}
        terms = [eye(p); pairTermsLocal(p)];
      case 'quadratic'
        terms = [eye(p); pairTermsLocal(p); 2 * eye(p)];
      case 'purequadratic'
        terms = [eye(p); 2 * eye(p)];
      otherwise
        error('Nelson:statistics:invalidModelSpecification', 'Invalid model specification.');
    end
  elseif isnumeric(modelspec)
    if size(modelspec, 2) ~= p
      error('Nelson:statistics:modelMatrixOneColumnPredictor', 'Model matrix must have one column per %s.', 'predictor');
    end
    terms = double(modelspec);
    terms(all(terms == 0, 2), :) = [];
  else
    error('Nelson:statistics:invalidModelSpecification', 'Invalid model specification.');
  end
end
%=============================================================================
function terms = pairTermsLocal(p)
  terms = zeros(0, p);
  for i = 1:p
    for j = i + 1:p
      row = zeros(1, p);
      row(i) = 1;
      row(j) = 1;
      terms = [terms; row];
    end
  end
end
%=============================================================================
function tbl = coefficientsTableLocal(fit, names)
  se = sqrt(diag(fit.Covariance));
  tStat = fit.Beta ./ se;
  if fit.DFE > 0
    pValue = 2 * (1 - tcdf(abs(tStat), fit.DFE));
  else
    pValue = NaN(size(tStat));
  end
  tbl = table(fit.Beta, se, tStat, pValue, 'VariableNames', {'Estimate', 'SE', 'tStat', 'pValue'}, ...
    'RowNames', names);
end
%=============================================================================
function names = coefficientNamesLocal(terms, intercept, predictorNames)
  names = {};
  if intercept
    names{end + 1} = '(Intercept)';
  end
  for k = 1:size(terms, 1)
    names{end + 1} = termNameLocal(terms(k, :), predictorNames);
  end
  names = names(:);
end
%=============================================================================
function name = termNameLocal(term, predictorNames)
  parts = {};
  for j = 1:length(term)
    if term(j) == 1
      parts{end + 1} = predictorNames{j};
    elseif term(j) > 1
      parts{end + 1} = [predictorNames{j}, '^', int2str(term(j))];
    end
  end
  name = parts{1};
  for k = 2:length(parts)
    name = [name, ':', parts{k}];
  end
end
%=============================================================================
function value = rsquaredLocal(fit)
  if fit.SST == 0
    ordinary = NaN;
  else
    ordinary = 1 - fit.SSE / fit.SST;
  end
  if fit.DFE > 0 && fit.NumObservations > 1 && ~isnan(ordinary)
    adjusted = 1 - (1 - ordinary) * (fit.NumObservations - 1) / fit.DFE;
  else
    adjusted = NaN;
  end
  value = struct('Ordinary', ordinary, 'Adjusted', adjusted);
end
%=============================================================================
function values = expandVectorLocal(values, keep, n)
  expanded = NaN(n, 1);
  expanded(keep) = values;
  values = expanded;
end
%=============================================================================
function names = predictorNamesLocal(names, p)
  if isempty(names)
    names = cell(p, 1);
    for k = 1:p
      names{k} = ['x', int2str(k)];
    end
  else
    if length(names) ~= p
      error('Nelson:statistics:predictorNamesOneNamePredictor', 'PredictorNames must have one name per predictor.');
    end
    names = names(:);
  end
end
%=============================================================================
function validateResponseLocal(y, distribution)
  switch distribution
    case 'binomial'
      if any(y < 0 | y > 1 | y ~= fix(y))
        error('Nelson:statistics:yContain01Values', 'Y must contain 0 or 1 values for the binomial distribution.');
      end
    case 'poisson'
      if any(y < 0 | y ~= fix(y))
        error('Nelson:statistics:yContainNonnegativeIntegerValues', 'Y must contain nonnegative integer values for the poisson distribution.');
      end
  end
end
%=============================================================================
function name = optionNameLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  names = {'intercept', 'predictornames', 'responsename', 'distribution', 'link', 'maxiter', 'tolfun'};
  exact = names(strcmp(names, text));
  if length(exact) == 1
    name = exact{1};
    return
  end
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  name = matches{1};
end
%=============================================================================
function name = distributionLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Distribution');
  end
  text = lower(char(value));
  names = {'normal', 'binomial', 'poisson'};
  exact = names(strcmp(names, text));
  if length(exact) == 1
    name = exact{1};
    return
  end
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidDistributionValue', 'Invalid %s value.', 'Distribution');
  end
  name = matches{1};
end
%=============================================================================
function name = linkLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Link');
  end
  text = lower(char(value));
  names = {'identity', 'log', 'logit'};
  exact = names(strcmp(names, text));
  if length(exact) == 1
    name = exact{1};
    return
  end
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidLinkValue', 'Invalid %s value.', 'Link');
  end
  name = matches{1};
end
%=============================================================================
function link = canonicalLinkLocal(distribution)
  switch distribution
    case 'binomial'
      link = 'logit';
    case 'poisson'
      link = 'log';
    otherwise
      link = 'identity';
  end
end
%=============================================================================
function validateDistributionLinkLocal(distribution, link)
  if strcmp(distribution, 'binomial') && ~strcmp(link, 'logit')
    error('Nelson:statistics:unsupportedLinkValueBinomialDistribution', 'Unsupported Link value for the %s distribution.', 'binomial');
  end
  if strcmp(distribution, 'poisson') && ~strcmp(link, 'log')
    error('Nelson:statistics:unsupportedLinkValuePoissonDistribution', 'Unsupported Link value for the %s distribution.', 'poisson');
  end
  if strcmp(distribution, 'normal') && ~any(strcmp(link, {'identity', 'log'}))
    error('Nelson:statistics:unsupportedLinkValueNormalDistribution', 'Unsupported Link value for the %s distribution.', 'normal');
  end
end
%=============================================================================
function name = responseNameLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'ResponseName');
  end
  name = char(value);
end
%=============================================================================
function value = textVectorLocal(value, name)
  if isstring(value)
    value = cellstr(value(:));
  elseif iscellstr(value)
    value = value(:);
  else
    error('Nelson:validators:mustBeTextArray', '%s must be a string array or cell array of character vectors.', name);
  end
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
