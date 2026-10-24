%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = ridge(y, X, k, scaled)
  narginchk(3, 4);
  nargoutchk(0, 1);
  if nargin < 4 || isempty(scaled)
    scaled = 1;
  end
  [y, X, k, scaled] = validateInputsLocal(y, X, k, scaled);
  keep = ~isnan(y) & all(~isnan(X), 2);
  B = ridgeFitLocal(y(keep), X(keep, :), k, scaled);
end
%=============================================================================
function [y, X, k, scaled] = validateInputsLocal(y, X, k, scaled)
  if ~(isnumeric(y) && isreal(y) && isvector(y) && ~issparse(y))
    error('Nelson:statistics:yRealNumericVector', '%s must be a real numeric vector.', 'Y');
  end
  if ~(isnumeric(X) && isreal(X) && ismatrix(X) && ~issparse(X))
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if ~(isnumeric(k) && isreal(k) && (isvector(k) || isempty(k)) ...
      && all(isfinite(k)) && all(k >= 0))
    error('Nelson:statistics:kNonnegativeNumericVector', '%s must be a nonnegative numeric vector.', 'K');
  end
  if ~((isnumeric(scaled) || islogical(scaled)) && isreal(scaled) && isscalar(scaled) ...
      && (scaled == 0 || scaled == 1))
    error('Nelson:statistics:SCALED01', '%s must be 0 or 1.', 'SCALED');
  end
  y = double(y(:));
  X = double(X);
  if size(X, 1) ~= length(y)
    error('Nelson:statistics:xYSameNumberRows', '%s must have the same number of rows.', 'Y and X');
  end
  k = double(k(:).');
  scaled = logical(scaled);
end
%=============================================================================
function B = ridgeFitLocal(y, X, k, scaled)
  p = size(X, 2);
  if isempty(k)
    if scaled
      B = zeros(p, 0);
    else
      B = zeros(p + 1, 0);
    end
    return
  end
  if isempty(y) || p == 0
    if scaled
      B = NaN(p, length(k));
    else
      B = NaN(p + 1, length(k));
    end
    return
  end
  xmean = mean(X, 1);
  ymean = mean(y);
  xscale = std(X, 0, 1);
  xscale(xscale == 0 | isnan(xscale)) = 1;
  Z = (X - xmean) ./ xscale;
  yc = y - ymean;
  Bscaled = zeros(p, length(k));
  XtX = Z' * Z;
  Xty = Z' * yc;
  I = eye(p, p);
  for j = 1:length(k)
    Bscaled(:, j) = (XtX + k(j) * I) \ Xty;
  end
  if scaled
    B = Bscaled;
  else
    Bunscaled = Bscaled ./ xscale(:);
    intercept = ymean - xmean * Bunscaled;
    B = [intercept; Bunscaled];
  end
end
%=============================================================================
