%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stats = regstats(y, X, modelspec, StatNames)
  narginchk(2, 4);
  nargoutchk(0, 1);
  if nargin < 3 || isempty(modelspec)
    modelspec = 'linear';
  end
  if nargin < 4 || isempty(StatNames)
    StatNames = 'all';
  end
  [y, X] = validateInputsLocal(y, X);
  keep = ~isnan(y) & all(~isnan(X), 2);
  D = designMatrixLocal(X(keep, :), modelspec);
  fit = fitLocal(y(keep), D);
  names = parseStatNamesLocal(StatNames);
  stats = collectStatsLocal(fit, names);
end
%=============================================================================
function [y, X] = validateInputsLocal(y, X)
  if ~(isnumeric(y) && isreal(y) && isvector(y) && ~issparse(y))
    error('Nelson:statistics:yRealNumericVector', '%s must be a real numeric vector.', 'Y');
  end
  if ~(isnumeric(X) && isreal(X) && ismatrix(X) && ~issparse(X))
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  y = double(y(:));
  X = double(X);
  if size(X, 1) ~= length(y)
    error('Nelson:statistics:xYSameNumberRows', '%s must have the same number of rows.', 'Y and X');
  end
end
%=============================================================================
function D = designMatrixLocal(X, modelspec)
  p = size(X, 2);
  terms = modelTermsLocal(p, modelspec);
  D = ones(size(X, 1), 1);
  for k = 1:size(terms, 1)
    col = ones(size(X, 1), 1);
    for j = 1:p
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
    if isscalar(modelspec)
      terms = degreeTermsLocal(p, positiveIntegerLocal(modelspec));
    else
      if size(modelspec, 2) ~= p
        error('Nelson:statistics:modelMatrixOneColumnPredictor', 'Model matrix must have one column per %s.', 'predictor');
      end
      terms = double(modelspec);
      terms(all(terms == 0, 2), :) = [];
    end
  else
    error('Nelson:statistics:invalidModelSpecification', 'Invalid model specification.');
  end
end
%=============================================================================
function terms = degreeTermsLocal(p, degree)
  terms = [];
  for total = 1:degree
    terms = [terms; compositionsLocal(p, total)];
  end
end
%=============================================================================
function terms = compositionsLocal(p, total)
  if p == 1
    terms = total;
    return
  end
  terms = [];
  for v = 0:total
    rest = compositionsLocal(p - 1, total - v);
    terms = [terms; [v * ones(size(rest, 1), 1) rest]];
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
function value = positiveIntegerLocal(value)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value > 0 && value == fix(value))
    error('Nelson:statistics:modelDegreePositiveIntegerScalar', '%s must be a positive integer scalar.', 'Model degree');
  end
  value = double(value);
end
%=============================================================================
function names = parseStatNamesLocal(StatNames)
  allNames = {'Q', 'R', 'beta', 'covb', 'yhat', 'r', 'mse', 'rsquare', ...
    'adjrsquare', 'leverage', 'hatmat', 's2_i', 'beta_i', 'standres', ...
    'studres', 'dfbetas', 'dffit', 'dffits', 'covratio', 'cookd', ...
    'tstat', 'fstat', 'dwstat'};
  if ischar(StatNames) || (isstring(StatNames) && isscalar(StatNames))
    requested = cellstr(char(StatNames));
  elseif isstring(StatNames)
    requested = cell(1, numel(StatNames));
    for k = 1:numel(StatNames)
      requested{k} = char(StatNames(k));
    end
  elseif iscell(StatNames)
    requested = StatNames(:).';
  else
    error('Nelson:statistics:statNamesCharacterVectorStringArray', 'StatNames must be a character vector, string array, or cell array.');
  end
  if any(strcmpi(requested, 'all'))
    names = allNames;
    return
  end
  names = cell(1, length(requested));
  for k = 1:length(requested)
    if ~(ischar(requested{k}) || (isstring(requested{k}) && isscalar(requested{k})))
      error('Nelson:statistics:statNamesEntriesTextScalars', 'StatNames entries must be text scalars.');
    end
    text = char(requested{k});
    idx = find(strcmp(text, allNames), 1);
    if isempty(idx)
      idx = find(strcmpi(text, allNames), 1);
    end
    if isempty(idx)
      error('Nelson:statistics:invalidStatisticName', 'Invalid statistic name.');
    end
    names{k} = allNames{idx};
  end
end
%=============================================================================
function fit = fitLocal(y, D)
  n = length(y);
  p = size(D, 2);
  fit = struct();
  fit.D = D;
  fit.Q = [];
  fit.R = [];
  if n == 0
    fit.beta = NaN(p, 1);
    fit.yhat = zeros(0, 1);
    fit.r = zeros(0, 1);
    fit.rank = 0;
  else
    fit.beta = D \ y;
    fit.yhat = D * fit.beta;
    fit.r = y - fit.yhat;
    fit.rank = rank(D);
  end
  fit.dfe = n - fit.rank;
  fit.sse = sum(fit.r .^ 2);
  fit.sst = sum((y - mean(y)) .^ 2);
  if fit.dfe > 0
    fit.mse = fit.sse / fit.dfe;
  else
    fit.mse = NaN;
  end
  fit.XtXinv = pinv(D' * D);
  fit.covb = fit.mse * fit.XtXinv;
  fit.H = [];
  fit.leverage = [];
  fit.n = n;
  fit.p = p;
end
%=============================================================================
function stats = collectStatsLocal(fit, names)
  stats = struct();
  stats.source = 'regstats';
  needsInfluence = any(ismember(names, {'leverage', 'hatmat', 's2_i', 'beta_i', ...
    'standres', 'studres', 'dfbetas', 'dffit', 'dffits', 'covratio', 'cookd'}));
  if any(ismember(names, {'Q', 'R'}))
    [fit.Q, fit.R] = qrLocal(fit.D);
  end
  if needsInfluence
    fit = influenceLocal(fit);
  end
  for k = 1:length(names)
    stats.(names{k}) = statValueLocal(fit, names{k});
  end
end
%=============================================================================
function value = statValueLocal(fit, name)
  switch name
    case {'Q', 'R', 'beta', 'covb', 'yhat', 'r', 'mse', 'leverage', 'hatmat'}
      value = fit.(name);
    case 'rsquare'
      value = rsquareLocal(fit);
    case 'adjrsquare'
      value = adjrsquareLocal(fit);
    case {'s2_i', 'beta_i', 'standres', 'studres', 'dfbetas', 'dffit', ...
        'dffits', 'covratio', 'cookd'}
      value = fit.(name);
    case 'tstat'
      value = tstatLocal(fit);
    case 'fstat'
      value = fstatLocal(fit);
    case 'dwstat'
      value = dwstatLocal(fit);
  end
end
%=============================================================================
function fit = influenceLocal(fit)
  fit.H = fit.D * fit.XtXinv * fit.D';
  fit.hatmat = fit.H;
  fit.leverage = diag(fit.H);
  oneMinusH = max(1 - fit.leverage, eps);
  if fit.dfe > 1
    fit.s2_i = (fit.sse - (fit.r .^ 2) ./ oneMinusH) ./ (fit.dfe - 1);
  else
    fit.s2_i = NaN(size(fit.r));
  end
  fit.standres = fit.r ./ sqrt(fit.mse .* oneMinusH);
  fit.studres = fit.r ./ sqrt(fit.s2_i .* oneMinusH);
  delta = fit.XtXinv * (fit.D' .* repmat((fit.r ./ oneMinusH).', fit.p, 1));
  fit.beta_i = repmat(fit.beta, 1, fit.n) - delta;
  scale = sqrt(diag(fit.XtXinv));
  fit.dfbetas = delta ./ (scale * sqrt(fit.s2_i).');
  fit.dffit = fit.leverage .* fit.r ./ oneMinusH;
  fit.dffits = fit.studres .* sqrt(fit.leverage ./ oneMinusH);
  fit.covratio = ((fit.s2_i ./ fit.mse) .^ fit.p) ./ oneMinusH;
  fit.cookd = (fit.r .^ 2 ./ (fit.p .* fit.mse)) .* fit.leverage ./ (oneMinusH .^ 2);
end
%=============================================================================
function [Q, R] = qrLocal(A)
  [n, p] = size(A);
  Q = zeros(n, p);
  R = zeros(p, p);
  for j = 1:p
    v = A(:, j);
    for i = 1:j - 1
      R(i, j) = Q(:, i)' * v;
      v = v - R(i, j) * Q(:, i);
    end
    R(j, j) = norm(v);
    if R(j, j) > eps
      Q(:, j) = v / R(j, j);
    end
  end
end
%=============================================================================
function value = rsquareLocal(fit)
  if fit.sst == 0
    value = NaN;
  else
    value = 1 - fit.sse / fit.sst;
  end
end
%=============================================================================
function value = adjrsquareLocal(fit)
  rsq = rsquareLocal(fit);
  if fit.dfe <= 0 || fit.n <= 1 || isnan(rsq)
    value = NaN;
  else
    value = 1 - (1 - rsq) * (fit.n - 1) / fit.dfe;
  end
end
%=============================================================================
function value = tstatLocal(fit)
  se = sqrt(diag(fit.covb));
  t = fit.beta ./ se;
  if fit.dfe > 0
    pval = 2 * (1 - tcdf(abs(t), fit.dfe));
  else
    pval = NaN(size(t));
  end
  value = struct('beta', fit.beta, 'se', se, 't', t, 'pval', pval);
end
%=============================================================================
function value = fstatLocal(fit)
  dfr = max(fit.rank - 1, 0);
  if dfr > 0 && fit.dfe > 0 && fit.sst ~= 0
    f = ((fit.sst - fit.sse) / dfr) / fit.mse;
    pval = fcdf(f, dfr, fit.dfe, 'upper');
  else
    f = NaN;
    pval = NaN;
  end
  value = struct('f', f, 'pval', pval, 'dfe', fit.dfe, 'dfr', dfr);
end
%=============================================================================
function value = dwstatLocal(fit)
  if fit.sse == 0
    dw = NaN;
  else
    dw = sum(diff(fit.r) .^ 2) / fit.sse;
  end
  value = struct('dw', dw, 'pval', NaN);
end
%=============================================================================
