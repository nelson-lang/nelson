%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [h, p, jbstat, critval] = jbtest(x, varargin)
  narginchk(1, 3);
  nargoutchk(0, 4);
  [alpha, mctol] = parseInputsLocal(varargin);
  values = validateSampleLocal(x);
  [jbstat, n] = statisticLocal(values);
  p = chi2cdf(jbstat, 2, 'upper');
  critval = chi2inv(1 - alpha, 2);
  h = p <= alpha;
  if n < 3 || isnan(jbstat)
    p = NaN;
    critval = NaN;
    h = false;
  end
  mctol;
end
%=============================================================================
function [alpha, mctol] = parseInputsLocal(args)
  alpha = 0.05;
  mctol = [];
  if length(args) >= 1 && ~isempty(args{1})
    alpha = scalarInRangeLocal(args{1}, 'ALPHA');
  end
  if length(args) >= 2
    mctol = nonnegativeScalarLocal(args{2}, 'MCTOL');
  end
end
%=============================================================================
function values = validateSampleLocal(x)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x) && isvector(x))
    error('Nelson:statistics:xRealNumericVector', '%s must be a real numeric vector.', 'X');
  end
  values = double(x(:));
  values = values(~isnan(values));
  if isempty(values)
    error('Nelson:statistics:xContainLeastOneNon', '%s must contain at least one non-NaN observation.', 'X');
  end
end
%=============================================================================
function [jbstat, n] = statisticLocal(values)
  n = length(values);
  mu = mean(values);
  centered = values - mu;
  m2 = mean(centered .^ 2);
  if m2 <= 0
    jbstat = 0;
    return
  end
  skewnessValue = mean(centered .^ 3) / (m2 ^ 1.5);
  kurtosisValue = mean(centered .^ 4) / (m2 ^ 2);
  jbstat = n / 6 * (skewnessValue ^ 2 + (kurtosisValue - 3) ^ 2 / 4);
end
%=============================================================================
function value = scalarInRangeLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && value > 0 && value < 1)
    error('Nelson:statistics:mustBeInRange', '%s must be a scalar in the range (0,1).', name);
  end
  value = double(value);
end
%=============================================================================
function value = nonnegativeScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && value >= 0)
    error('Nelson:statistics:mustBeNonnegative', '%s must be a nonnegative scalar.', name);
  end
  value = double(value);
end
%=============================================================================
