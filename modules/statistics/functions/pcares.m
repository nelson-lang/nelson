%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [residuals, reconstructed] = pcares(X, NumComponents)
  narginchk(2, 2);
  nargoutchk(0, 2);
  preserveSingle = isa(X, 'single');
  X = validateDataLocal(X);
  NumComponents = validateNumComponentsLocal(NumComponents, size(X, 2));
  if NumComponents == 0
    mu = mean(X, 1);
    reconstructed = repmat(mu, size(X, 1), 1);
  else
    [coeff, score, ~, ~, ~, mu] = pca(X, 'NumComponents', NumComponents);
    reconstructed = score * coeff' + repmat(mu, size(X, 1), 1);
  end
  residuals = X - reconstructed;
  if preserveSingle
    residuals = single(residuals);
    reconstructed = single(reconstructed);
  end
end
%=============================================================================
function X = validateDataLocal(X)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if isempty(X) || size(X, 2) == 0
    error('Nelson:statistics:xLeastOneVariable', 'X must have at least one variable.');
  end
  X = double(X);
  if any(isnan(X(:)))
    error('Nelson:statistics:xContainNaNValues', 'X must not contain NaN values.');
  end
end
%=============================================================================
function value = validateNumComponentsLocal(value, nvars)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value >= 0 && floor(value) == value && value <= nvars)
    error('Nelson:statistics:numComponentsIntegerScalarBetween0', 'NumComponents must be an integer scalar between 0 and the number of variables.');
  end
  value = double(value);
end
%=============================================================================
