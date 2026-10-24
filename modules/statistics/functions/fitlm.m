%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitlm(X, y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, y] = validateInputsLocal(X, y);
  [modelspec, args] = parseModelSpecLocal(varargin);
  opts = parseOptionsLocal(args);
  terms = modelTermsLocal(size(X, 2), modelspec);
  keep = ~isnan(y) & all(~isnan(X), 2);
  fit = fitLocal(X(keep, :), y(keep), terms, opts.Intercept);
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
  if ~isempty(args)
    first = args{1};
    if isModelSpecValueLocal(first)
      modelspec = first;
      args = args(2:end);
    end
  end
end
%=============================================================================
function tf = isModelSpecValueLocal(value)
  tf = isnumeric(value) && ~isscalar(value);
  if ischar(value) || (isstring(value) && isscalar(value))
    text = lower(char(value));
    specs = {'constant', 'linear', 'additive', 'interactions', 'interaction', ...
      'quadratic', 'purequadratic'};
    tf = any(strcmp(text, specs));
  end
end
%=============================================================================
function opts = parseOptionsLocal(args)
  opts = struct('Intercept', true, 'PredictorNames', [], 'ResponseName', 'y');
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
        if ~(ischar(value) || (isstring(value) && isscalar(value)))
          error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'ResponseName');
        end
        opts.ResponseName = char(value);
    end
  end
end
%=============================================================================
function fit = fitLocal(X, y, terms, intercept)
  D = designMatrixLocal(X, terms, intercept);
  n = length(y);
  p = size(D, 2);
  beta = D \ y;
  yhat = D * beta;
  r = y - yhat;
  rankD = rank(D);
  dfe = n - rankD;
  sse = sum(r .^ 2);
  sst = sum((y - mean(y)) .^ 2);
  mse = NaN;
  if dfe > 0
    mse = sse / dfe;
  end
  XtXinv = pinv(D' * D);
  fit = struct('D', D, 'Beta', beta, 'Fitted', yhat, 'Residuals', r, 'Rank', rankD, ...
    'DFE', dfe, 'SSE', sse, 'SST', sst, 'MSE', mse, 'Covariance', mse * XtXinv, ...
    'NumCoefficients', p, 'NumObservations', n);
end
%=============================================================================
function mdl = buildModelLocal(fit, X, y, keep, terms, opts)
  mdl = LinearModel();
  mdl.Intercept = opts.Intercept;
  mdl.Terms = terms;
  mdl.Beta = fit.Beta;
  mdl.Covariance = fit.Covariance;
  mdl.NumObservations = fit.NumObservations;
  mdl.NumPredictors = size(X, 2);
  mdl.DFE = fit.DFE;
  mdl.RMSE = sqrt(fit.MSE);
  mdl.SSE = fit.SSE;
  mdl.SST = fit.SST;
  mdl.SSR = fit.SST - fit.SSE;
  mdl.Rsquared = rsquaredLocal(fit);
  mdl.FStat = fstatLocal(fit);
  mdl.ModelCriterion = modelCriterionLocal(fit);
  mdl.PredictorNames = predictorNamesLocal(opts.PredictorNames, size(X, 2));
  mdl.ResponseName = opts.ResponseName;
  mdl.CoefficientNames = coefficientNamesLocal(terms, opts.Intercept, mdl.PredictorNames);
  mdl.Coefficients = coefficientsTableLocal(fit, mdl.CoefficientNames);
  mdl.Fitted = expandVectorLocal(fit.Fitted, keep, size(X, 1));
  residuals = expandVectorLocal(fit.Residuals, keep, size(X, 1));
  mdl.Residuals = table(residuals, 'VariableNames', {'Raw'});
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
function value = fstatLocal(fit)
  df1 = max(fit.Rank - 1, 0);
  if df1 > 0 && fit.DFE > 0 && fit.MSE > 0
    f = ((fit.SST - fit.SSE) / df1) / fit.MSE;
    p = 1 - fcdf(f, df1, fit.DFE);
  else
    f = NaN;
    p = NaN;
  end
  value = struct('F', f, 'pValue', p, 'DF1', df1, 'DF2', fit.DFE);
end
%=============================================================================
function value = modelCriterionLocal(fit)
  n = fit.NumObservations;
  k = fit.NumCoefficients;
  if n == 0 || fit.SSE <= 0
    aic = NaN;
    bic = NaN;
  else
    logL = -0.5 * n * (log(2 * pi) + 1 + log(fit.SSE / n));
    aic = -2 * logL + 2 * k;
    bic = -2 * logL + k * log(n);
  end
  value = struct('AIC', aic, 'BIC', bic);
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
function name = optionNameLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  text = lower(char(value));
  names = {'intercept', 'predictornames', 'responsename'};
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  name = matches{1};
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
