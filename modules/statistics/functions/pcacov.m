%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [coeff, latent, explained] = pcacov(V)
  narginchk(1, 1);
  nargoutchk(0, 3);
  preserveSingle = isa(V, 'single');
  V = validateCovarianceLocal(V);
  if isempty(V)
    coeff = zeros(0, 0);
    latent = zeros(0, 1);
    explained = zeros(0, 1);
    return
  end
  [coeff, latent] = decomposeLocal(V);
  explained = explainedLocal(latent);
  if preserveSingle
    coeff = single(coeff);
    latent = single(latent);
    explained = single(explained);
  end
end
%=============================================================================
function V = validateCovarianceLocal(V)
  if ~(isnumeric(V) && isreal(V) && ~issparse(V) && ndims(V) == 2)
    error('Nelson:statistics:vRealNumericMatrix', '%s must be a real numeric matrix.', 'V');
  end
  if size(V, 1) ~= size(V, 2)
    error('Nelson:statistics:vSquareMatrix', '%s must be a square matrix.', 'V');
  end
  V = double(V);
  if any(~isfinite(V(:)))
    error('Nelson:statistics:vContainFiniteValues', '%s must contain finite values.', 'V');
  end
  if isempty(V)
    return
  end
  tolerance = 100 * eps(max(1, max(abs(V(:)))));
  if any(any(abs(V - V') > tolerance))
    error('Nelson:statistics:vSymmetric', '%s must be symmetric.', 'V');
  end
  V = (V + V') / 2;
end
%=============================================================================
function [coeff, latent] = decomposeLocal(V)
  [coeff, D] = eig(V);
  latent = real(diag(D));
  [latent, order] = sort(latent, 'descend');
  coeff = real(coeff(:, order));
  tolerance = 100 * eps(max(1, max(abs(latent))));
  if any(latent < -tolerance)
    error('Nelson:statistics:vPositiveSemidefinite', 'V must be positive semidefinite.');
  end
  latent(latent < 0) = 0;
  coeff = orientColumnsLocal(coeff);
end
%=============================================================================
function coeff = orientColumnsLocal(coeff)
  for k = 1:size(coeff, 2)
    [~, idx] = max(abs(coeff(:, k)));
    if coeff(idx, k) < 0
      coeff(:, k) = -coeff(:, k);
    end
  end
end
%=============================================================================
function explained = explainedLocal(latent)
  total = sum(latent);
  if total > 0
    explained = 100 * latent(:) / total;
  else
    explained = zeros(size(latent(:)));
  end
end
%=============================================================================
