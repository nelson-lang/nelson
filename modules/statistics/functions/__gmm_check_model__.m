%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [mu, Sigma, p] = __gmm_check_model__(mu, Sigma, p)
  if nargin < 3 || isempty(p)
    p = ones(1, size(mu, 1)) / size(mu, 1);
  end
  mu = checkMu(mu);
  p = checkProportions(p, size(mu, 1));
  Sigma = checkSigma(Sigma, size(mu, 2), size(mu, 1));
end
%=============================================================================
function mu = checkMu(mu)
  if ~(isnumeric(mu) && isreal(mu) && ~issparse(mu) && ndims(mu) == 2)
    error('Nelson:statistics:MURealNumericMatrix', '%s must be a real numeric matrix.', 'MU');
  end
  mu = double(mu);
  if isempty(mu) || any(~isfinite(mu(:)))
    error('Nelson:statistics:MUContainFiniteValues', '%s must contain finite values.', 'MU');
  end
end
%=============================================================================
function p = checkProportions(p, k)
  if ~(isnumeric(p) && isreal(p) && ~issparse(p) && isvector(p))
    error('Nelson:statistics:componentProportionsNumericVector', 'Component proportions must be a numeric vector.');
  end
  p = double(p(:)');
  if length(p) ~= k || any(~isfinite(p)) || any(p < 0) || sum(p) <= 0
    error('Nelson:statistics:componentProportionsNonnegativeCompatibleMU', 'Component proportions must be nonnegative and compatible with MU.');
  end
  p = p / sum(p);
end
%=============================================================================
function Sigma = checkSigma(Sigma, d, k)
  if ~(isnumeric(Sigma) && isreal(Sigma) && ~issparse(Sigma))
    error('Nelson:statistics:SIGMARealNumericArray', '%s must be a real numeric array.', 'SIGMA');
  end
  Sigma = double(Sigma);
  if any(~isfinite(Sigma(:)))
    error('Nelson:statistics:SIGMAContainFiniteValues', '%s must contain finite values.', 'SIGMA');
  end
  Sigma = normalizeSigmaShape(Sigma, d, k);
  for j = 1:k
    if min(eig(Sigma(:, :, j))) <= 0
      error('Nelson:statistics:SIGMAContainPositiveDefiniteCovariance', 'SIGMA must contain positive definite covariance matrices.');
    end
  end
end
%=============================================================================
function Sigma = normalizeSigmaShape(Sigma, d, k)
  if isvector(Sigma) && length(Sigma) == d
    Sigma = repmat(diag(Sigma(:)), [1 1 k]);
  elseif ndims(Sigma) == 2 && isequal(size(Sigma), [d k])
    Sigma = diagonalPages(Sigma);
  elseif ndims(Sigma) == 2 && isequal(size(Sigma), [d d])
    Sigma = repmat(Sigma, [1 1 k]);
  elseif ndims(Sigma) == 3 && isequal(size(Sigma), [d d k])
    return
  else
    error('Nelson:statistics:SIGMAIncompatibleSize', '%s has incompatible size.', 'SIGMA');
  end
end
%=============================================================================
function pages = diagonalPages(values)
  d = size(values, 1);
  k = size(values, 2);
  pages = zeros(d, d, k);
  for j = 1:k
    pages(:, :, j) = diag(values(:, j));
  end
end
%=============================================================================
