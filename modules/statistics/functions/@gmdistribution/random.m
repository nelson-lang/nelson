%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function X = random(obj, n)
  narginchk(2, 2);
  nargoutchk(0, 1);
  if ~(isnumeric(n) && isreal(n) && isscalar(n) && isfinite(n) && floor(n) == n && n >= 0)
    error('Nelson:statistics:nNonnegativeIntegerScalar', '%s must be a nonnegative integer scalar.', 'N');
  end
  n = double(n);
  labels = sampleComponents(obj.ComponentProportion, n);
  X = zeros(n, obj.NumVariables);
  for j = 1:obj.NumComponents
    rows = find(labels == j);
    if ~isempty(rows)
      X(rows, :) = randn(length(rows), obj.NumVariables) * chol(obj.Sigma(:, :, j)) ...
        + repmat(obj.mu(j, :), length(rows), 1);
    end
  end
end
%=============================================================================
function labels = sampleComponents(p, n)
  edges = cumsum(p(:)');
  u = rand(n, 1);
  labels = ones(n, 1);
  for j = 1:length(edges)
    labels(u > edges(j)) = j + 1;
  end
end
%=============================================================================
