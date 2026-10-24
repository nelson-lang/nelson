%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [b, bint, r, rint, stats] = regress(y, X, alpha)
  narginchk(2, 3);
  nargoutchk(0, 5);
  if nargin < 3
    alpha = 0.05;
  end
  [y, X, alpha] = validateInputsLocal(y, X, alpha);
  keep = ~isnan(y) & all(~isnan(X), 2);
  yfit = y(keep);
  Xfit = X(keep, :);
  [b, bintFit, rFit, rintFit, stats] = regressFitLocal(yfit, Xfit, alpha);
  r = NaN(size(y));
  rint = NaN(length(y), 2);
  r(keep) = rFit;
  rint(keep, :) = rintFit;
  bint = bintFit;
end
%=============================================================================
function [y, X, alpha] = validateInputsLocal(y, X, alpha)
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
  if ~(isnumeric(alpha) && isreal(alpha) && isscalar(alpha) && isfinite(alpha) ...
      && alpha > 0 && alpha < 1)
    error('Nelson:statistics:alphaOutOfRange', '%s must be a scalar in the range (0,1).', 'ALPHA');
  end
  alpha = double(alpha);
end
%=============================================================================
function [b, bint, r, rint, stats] = regressFitLocal(y, X, alpha)
  n = length(y);
  p = size(X, 2);
  if n == 0 || p == 0
    b = NaN(p, 1);
    bint = NaN(p, 2);
    r = NaN(n, 1);
    rint = NaN(n, 2);
    stats = NaN(1, 4);
    return
  end
  b = X \ y;
  r = y - X * b;
  df = n - rank(X);
  sse = sum(r .^ 2);
  if df <= 0
    mse = NaN;
  else
    mse = sse / df;
  end
  XtXinv = pinv(X' * X);
  bint = coefficientIntervalsLocal(b, XtXinv, mse, df, alpha);
  rint = residualIntervalsLocal(r, X, XtXinv, mse, df, alpha);
  stats = modelStatsLocal(y, r, p, df, mse);
end
%=============================================================================
function bint = coefficientIntervalsLocal(b, XtXinv, mse, df, alpha)
  if df <= 0 || isnan(mse)
    bint = NaN(length(b), 2);
    return
  end
  se = sqrt(mse * diag(XtXinv));
  delta = tinv(1 - alpha / 2, df) * se;
  bint = [b - delta, b + delta];
end
%=============================================================================
function rint = residualIntervalsLocal(r, X, XtXinv, mse, df, alpha)
  if df <= 0 || isnan(mse)
    rint = NaN(length(r), 2);
    return
  end
  h = diag(X * XtXinv * X');
  se = sqrt(max(0, mse * (1 - h)));
  delta = tinv(1 - alpha / 2, df) * se;
  rint = [r - delta, r + delta];
end
%=============================================================================
function stats = modelStatsLocal(y, r, p, df, mse)
  sse = sum(r .^ 2);
  sst = sum((y - mean(y)) .^ 2);
  if sst == 0
    r2 = NaN;
  else
    r2 = 1 - sse / sst;
  end
  regressionDf = p - 1;
  if regressionDf <= 0 || df <= 0 || isnan(mse)
    fstat = NaN;
    pvalue = NaN;
  else
    fstat = ((sst - sse) / regressionDf) / mse;
    pvalue = 1 - fcdf(fstat, regressionDf, df);
  end
  stats = [r2, fstat, pvalue, mse];
end
%=============================================================================
