%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [weighted, density] = __gmm_weighted_density__(X, mu, Sigma, p)
  X = __gmm_check_data__(X);
  [mu, Sigma, p] = __gmm_check_model__(mu, Sigma, p);
  if size(X, 2) ~= size(mu, 2)
    error('Nelson:statistics:xSameNumberColumnsMU', 'X must have the same number of columns as %s.', 'MU');
  end
  n = size(X, 1);
  k = size(mu, 1);
  density = zeros(n, k);
  for j = 1:k
    density(:, j) = componentDensity(X, mu(j, :), Sigma(:, :, j));
  end
  weighted = density .* repmat(p, n, 1);
end
%=============================================================================
function y = componentDensity(X, center, covariance)
  d = size(X, 2);
  R = chol(covariance);
  centered = X - repmat(center, size(X, 1), 1);
  z = centered / R;
  q = (z .* z) * ones(size(z, 2), 1);
  scale = (2 * pi) ^ (d / 2) * prod(diag(R));
  y = exp(-0.5 * q) / scale;
end
%=============================================================================
