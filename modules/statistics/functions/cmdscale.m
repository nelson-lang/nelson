%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [Y, e] = cmdscale(D, p)
  narginchk(1, 2);
  nargoutchk(0, 2);
  if nargin < 2
    p = [];
  end
  [D, requestedP] = parseInputsLocal(D, nargin, p);
  n = size(D, 1);
  if n == 0
    Y = zeros(0, 0);
    e = zeros(0, 1);
    return
  end
  D = dissimilarityMatrixLocal(D);
  B = gramMatrixLocal(D);
  [V, E] = eig((B + B') / 2);
  [eAll, order] = sort(real(diag(E)), 'descend');
  V = real(V(:, order));
  V = orientColumnsLocal(V);
  positive = positiveEigenvaluesLocal(eAll);
  if isempty(requestedP)
    e = eAll(:);
    cols = find(positive);
  else
    p = requestedP;
    e = eAll(1:min(p, length(eAll)));
    cols = find(positive(1:min(p, length(positive))));
  end
  if isempty(cols)
    Y = zeros(n, 0);
  else
    Y = V(:, cols) * diag(sqrt(max(eAll(cols), 0)));
  end
end
%=============================================================================
function [D, p] = parseInputsLocal(D, nin, inputP)
  if ~(isnumeric(D) && isreal(D) && ~issparse(D) && ndims(D) == 2)
    error('Nelson:statistics:dRealNumericVectorMatrix', '%s must be a real numeric vector or matrix.', 'D');
  end
  if any(~isfinite(double(D(:))))
    error('Nelson:statistics:dContainFiniteValues', '%s must contain finite values.', 'D');
  end
  D = double(D);
  p = [];
  if isvector(D) && ~(size(D, 1) == size(D, 2))
    D = squareform(D);
  elseif size(D, 1) ~= size(D, 2)
    error('Nelson:statistics:dSquareValidDistanceVector', 'D must be square or a valid distance vector.');
  end
  if nin == 2
    p = validatePRequestedLocal(inputP, size(D, 1));
  end
end
%=============================================================================
function p = validatePRequestedLocal(p, n)
  if ~(isnumeric(p) && isreal(p) && isscalar(p) && isfinite(p) ...
      && floor(p) == p && p >= 1 && p <= n)
    error('Nelson:statistics:pIntegerScalarBetween1', 'p must be an integer scalar between 1 and the number of points.');
  end
  p = double(p);
end
%=============================================================================
function D = dissimilarityMatrixLocal(D)
  if isempty(D)
    return
  end
  if isSimilarityLocal(D)
    D = sqrt(max(0, 1 - D));
    D(1:size(D, 1) + 1:end) = 0;
    return
  end
  if isUpperTriangularDissimilarityLocal(D)
    D = D + D';
  end
  validateDissimilarityLocal(D);
end
%=============================================================================
function tf = isSimilarityLocal(D)
  diagonal = diag(D);
  offDiagonal = D - diag(diagonal);
  tf = all(abs(diagonal - 1) <= 100 * eps) ...
    && all(offDiagonal(:) <= 1 + 100 * eps) ...
    && isSymmetricLocal(D);
end
%=============================================================================
function tf = isUpperTriangularDissimilarityLocal(D)
  diagonal = diag(D);
  lower = tril(D, -1);
  upper = triu(D, 1);
  tf = all(abs(diagonal) <= 100 * eps) ...
    && all(abs(lower(:)) <= 100 * eps) ...
    && any(upper(:) > 0);
end
%=============================================================================
function validateDissimilarityLocal(D)
  if ~isSymmetricLocal(D)
    error('Nelson:statistics:dSymmetricUpperTriangular', 'D must be symmetric or upper triangular.');
  end
  if any(abs(diag(D)) > 100 * eps)
    error('Nelson:statistics:dZerosDiagonal', 'D must have zeros on the diagonal.');
  end
  if any(D(:) < -100 * eps)
    error('Nelson:statistics:dContainNonnegativeDissimilarities', 'D must contain nonnegative dissimilarities.');
  end
end
%=============================================================================
function tf = isSymmetricLocal(D)
  tolerance = 100 * eps(max(1, max(abs(D(:)))));
  tf = all(all(abs(D - D') <= tolerance));
end
%=============================================================================
function B = gramMatrixLocal(D)
  n = size(D, 1);
  D2 = D .^ 2;
  rowMean = mean(D2, 2);
  colMean = mean(D2, 1);
  totalMean = mean(D2(:));
  B = -0.5 * (D2 - repmat(rowMean, 1, n) - repmat(colMean, n, 1) + totalMean);
end
%=============================================================================
function positive = positiveEigenvaluesLocal(e)
  tolerance = max(size(e)) * eps(max(1, max(abs(e))));
  positive = e > tolerance;
end
%=============================================================================
function V = orientColumnsLocal(V)
  for k = 1:size(V, 2)
    [~, idx] = max(abs(V(:, k)));
    if V(idx, k) < 0
      V(:, k) = -V(:, k);
    end
  end
end
%=============================================================================
