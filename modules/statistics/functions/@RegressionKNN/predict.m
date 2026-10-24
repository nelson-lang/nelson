%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [yfit, D] = predict(obj, X)
  narginchk(2, 2);
  nargoutchk(0, 2);
  X = validatePredictorsLocal(X, obj.NumPredictors);
  Xq = standardizeLocal(double(X), obj);
  [idx, D] = neighborSearchLocal(obj, Xq);
  yfit = responseMeanLocal(obj, idx, D);
end
%=============================================================================
function X = validatePredictorsLocal(X, p)
  if ~(isnumeric(X) && isreal(X) && ismatrix(X) && ~issparse(X))
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if size(X, 2) ~= p
    error('Nelson:statistics:xSameNumberColumnsTraining', 'X must have the same number of columns as %s.', 'the training data');
  end
  if any(~isfinite(X(:)))
    error('Nelson:statistics:xMustBeFinite', '%s must contain finite values.', 'X');
  end
end
%=============================================================================
function Xq = standardizeLocal(X, obj)
  Xq = X;
  if obj.Standardize
    Xq = (Xq - repmat(obj.Mu, size(Xq, 1), 1)) ./ repmat(obj.Sigma, size(Xq, 1), 1);
  end
end
%=============================================================================
function [idx, D] = neighborSearchLocal(obj, Xq)
  args = {'K', obj.NumNeighbors, 'Distance', obj.Distance};
  if strcmp(obj.Distance, 'minkowski')
    args = [args, {'P', obj.P}];
  elseif strcmp(obj.Distance, 'seuclidean') && ~isempty(obj.Scale)
    args = [args, {'Scale', obj.Scale}];
  end
  [idx, D] = knnsearch(obj.X, Xq, args{:});
end
%=============================================================================
function yfit = responseMeanLocal(obj, idx, D)
  weights = distanceWeightsLocal(D, obj.DistanceWeight);
  yfit = zeros(size(idx, 1), 1);
  for row = 1:size(idx, 1)
    values = obj.Y(idx(row, :));
    w = weights(row, :).';
    yfit(row) = sum(values .* w) / sum(w);
  end
end
%=============================================================================
function weights = distanceWeightsLocal(D, method)
  switch method
    case 'equal'
      weights = ones(size(D));
    case 'inverse'
      weights = 1 ./ max(D, eps);
    otherwise
      weights = 1 ./ max(D, eps) .^ 2;
  end
end
%=============================================================================
