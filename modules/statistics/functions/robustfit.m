%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [b, stats] = robustfit(X, y, wfun, tune, const)
  narginchk(2, 5);
  nargoutchk(0, 2);
  if nargin < 3 || isempty(wfun)
    wfun = 'bisquare';
  end
  if nargin < 4
    tune = [];
  end
  if nargin < 5 || isempty(const)
    const = 'on';
  end
  [X, y, wfun, tune, const] = validateInputsLocal(X, y, wfun, tune, const);
  if strcmp(const, 'on')
    X = [ones(size(X, 1), 1), X];
  end
  keep = ~isnan(y) & all(~isnan(X), 2);
  [b, fitStats] = robustFitLocal(X(keep, :), y(keep), wfun, tune);
  stats = expandStatsLocal(fitStats, keep, size(X, 1));
end
%=============================================================================
function [X, y, wfun, tune, const] = validateInputsLocal(X, y, wfun, tune, const)
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
  const = parseConstLocal(const);
  wfun = parseWeightFunctionLocal(wfun);
  tune = parseTuneLocal(tune, wfun);
end
%=============================================================================
function const = parseConstLocal(value)
  if ~isTextScalarLocal(value)
    error('Nelson:statistics:CONSTOff', 'CONST must be ''on'' or ''off''.');
  end
  const = parseChoiceLocal(value, {'on', 'off'}, 'CONST');
end
%=============================================================================
function tune = parseTuneLocal(tune, wfun)
  if isempty(tune)
    tune = defaultTuneLocal(wfun);
    return
  end
  if ~(isnumeric(tune) && isreal(tune) && isscalar(tune) && isfinite(tune) && tune > 0)
    error('Nelson:statistics:TUNEPositiveScalar', '%s must be a positive scalar.', 'TUNE');
  end
  tune = double(tune);
end
%=============================================================================
function name = parseWeightFunctionLocal(wfun)
  if isa(wfun, 'function_handle')
    name = wfun;
    return
  end
  if ~isTextScalarLocal(wfun)
    error('Nelson:statistics:WFUNCharacterVectorStringScalar', 'WFUN must be a character vector, string scalar, or function handle.');
  end
  name = parseChoiceLocal(wfun, {'andrews', 'bisquare', 'cauchy', 'fair', ...
                                 'huber', 'logistic', 'ols', 'talwar', 'welsch'}, 'WFUN');
end
%=============================================================================
function [b, stats] = robustFitLocal(X, y, wfun, tune)
  n = length(y);
  p = size(X, 2);
  if n == 0 || p == 0
    b = NaN(p, 1);
    stats = emptyStatsLocal(n, p);
    return
  end
  b = X \ y;
  dfe = n - rank(X);
  h = leverageLocal(X);
  resid = y - X * b;
  ols_s = scaleFromResidualsLocal(resid, dfe);
  if ischar(wfun) && strcmp(wfun, 'ols')
    w = ones(n, 1);
  else
    [b, resid, w] = irlsLocal(X, y, b, h, wfun, tune, p);
  end
  stats = buildStatsLocal(X, y, b, resid, w, h, dfe, ols_s);
end
%=============================================================================
function [b, resid, w] = irlsLocal(X, y, b, h, wfun, tune, p)
  w = ones(length(y), 1);
  for iter = 1:50
    resid = y - X * b;
    s = madScaleLocal(resid, p);
    u = resid ./ max(tune * s * sqrt(max(1 - h, eps)), eps);
    w = weightValuesLocal(u, wfun);
    sw = sqrt(max(w, 0));
    bnew = (X .* sw) \ (y .* sw);
    if norm(bnew - b) <= 1e-6 * max(norm(b), eps)
      b = bnew;
      break
    end
    b = bnew;
  end
  resid = y - X * b;
end
%=============================================================================
function stats = buildStatsLocal(X, y, b, resid, w, h, dfe, ols_s)
  p = length(b);
  mad_s = madScaleLocal(resid, p);
  robust_s = scaleFromResidualsLocal(sqrt(max(w, 0)) .* resid, dfe);
  s = max(mad_s, robust_s);
  covb = pinv(X' * X) * s ^ 2;
  se = sqrt(max(0, diag(covb)));
  tval = b ./ se;
  pval = 2 * (1 - tcdf(abs(tval), max(dfe, 1)));
  stats = struct();
  stats.ols_s = ols_s;
  stats.robust_s = robust_s;
  stats.mad_s = mad_s;
  stats.s = s;
  stats.resid = resid;
  stats.rstud = resid ./ max(s * sqrt(max(1 - h, eps)), eps);
  stats.se = se;
  stats.covb = covb;
  stats.coeffcorr = coefficientCorrelationLocal(covb);
  stats.t = tval;
  stats.p = pval;
  stats.w = w;
  stats.R = rFactorLocal(X);
  stats.dfe = dfe;
  stats.h = h;
end
%=============================================================================
function stats = expandStatsLocal(stats, keep, n)
  fields = {'resid', 'rstud', 'w', 'h'};
  for k = 1:length(fields)
    name = fields{k};
    values = NaN(n, 1);
    values(keep) = stats.(name);
    stats.(name) = values;
  end
end
%=============================================================================
function stats = emptyStatsLocal(n, p)
  stats = struct('ols_s', NaN, 'robust_s', NaN, 'mad_s', NaN, 's', NaN, ...
    'resid', NaN(n, 1), 'rstud', NaN(n, 1), 'se', NaN(p, 1), ...
    'covb', NaN(p, p), 'coeffcorr', NaN(p, p), 't', NaN(p, 1), ...
    'p', NaN(p, 1), 'w', NaN(n, 1), 'R', NaN(p, p), 'dfe', NaN, ...
    'h', NaN(n, 1));
end
%=============================================================================
function s = madScaleLocal(resid, p)
  deviations = sort(abs(resid - median(resid)));
  if length(deviations) > p
    deviations = deviations(p + 1:end);
  end
  s = median(deviations) / 0.6745;
  if ~(isfinite(s) && s > 0)
    s = max(scaleFromResidualsLocal(resid, length(resid) - p), eps);
  end
end
%=============================================================================
function s = scaleFromResidualsLocal(resid, dfe)
  if dfe <= 0
    s = NaN;
  else
    s = sqrt(sum(resid .^ 2) / dfe);
  end
end
%=============================================================================
function h = leverageLocal(X)
  h = diag(X * pinv(X' * X) * X');
  h = min(max(h, 0), 1);
end
%=============================================================================
function w = weightValuesLocal(u, wfun)
  if isa(wfun, 'function_handle')
    w = feval(wfun, u);
    return
  end
  switch wfun
    case 'andrews'
      w = ones(size(u));
      nz = u ~= 0;
      w(nz) = (abs(u(nz)) < pi) .* sin(u(nz)) ./ u(nz);
    case 'bisquare'
      w = (abs(u) < 1) .* (1 - u .^ 2) .^ 2;
    case 'cauchy'
      w = 1 ./ (1 + u .^ 2);
    case 'fair'
      w = 1 ./ (1 + abs(u));
    case 'huber'
      w = 1 ./ max(1, abs(u));
    case 'logistic'
      w = ones(size(u));
      nz = u ~= 0;
      w(nz) = tanh(u(nz)) ./ u(nz);
    case 'talwar'
      w = double(abs(u) < 1);
    otherwise
      w = exp(-(u .^ 2));
  end
  w(~isfinite(w)) = 1;
end
%=============================================================================
function c = coefficientCorrelationLocal(covb)
  d = sqrt(diag(covb));
  c = covb ./ (d * d');
end
%=============================================================================
function R = rFactorLocal(X)
  try
    R = chol(X' * X);
  catch
    R = [];
  end
end
%=============================================================================
function tune = defaultTuneLocal(wfun)
  if isa(wfun, 'function_handle')
    tune = 1;
    return
  end
  switch wfun
    case 'andrews'
      tune = 1.339;
    case 'cauchy'
      tune = 2.385;
    case 'fair'
      tune = 1.400;
    case 'huber'
      tune = 1.345;
    case 'logistic'
      tune = 1.205;
    case 'talwar'
      tune = 2.795;
    case 'welsch'
      tune = 2.985;
    otherwise
      tune = 4.685;
  end
end
%=============================================================================
function value = parseChoiceLocal(value, choices, name)
  text = lower(char(value));
  matches = choices(startsWith(choices, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
  end
  value = matches{1};
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
