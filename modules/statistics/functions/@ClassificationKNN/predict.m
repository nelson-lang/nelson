%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [label, score, cost] = predict(obj, X)
  narginchk(2, 2);
  nargoutchk(0, 3);
  X = validatePredictorsLocal(X, obj.NumPredictors);
  Xq = standardizeLocal(double(X), obj);
  [idx, D] = neighborSearchLocal(obj, Xq);
  [classIndex, score] = voteLocal(obj, idx, D);
  label = labelsFromIndexLocal(obj, classIndex, size(X, 1));
  cost = 1 - score;
end
%=============================================================================
function X = validatePredictorsLocal(X, p)
  if ~(isnumeric(X) && isreal(X) && ismatrix(X) && ~issparse(X))
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if size(X, 2) ~= p
    error('Nelson:statistics:xSameNumberColumnsTraining', 'X must have the same number of columns as %s.', 'the training data');
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
function [classIndex, score] = voteLocal(obj, idx, D)
  n = size(idx, 1);
  kClasses = length(obj.ClassNames);
  score = zeros(n, kClasses);
  weights = distanceWeightsLocal(D, obj.DistanceWeight);
  for row = 1:n
    classes = obj.Y(idx(row, :));
    for j = 1:kClasses
      score(row, j) = sum(weights(row, classes == j));
    end
    total = sum(score(row, :));
    if total > 0
      score(row, :) = score(row, :) / total;
    end
  end
  [~, classIndex] = max(score, [], 2);
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
function labels = labelsFromIndexLocal(obj, classIndex, n)
  template = obj.ClassNames;
  if isnumeric(template)
    labels = reshape(template(classIndex), n, 1);
  elseif islogical(template)
    labels = reshape(template(classIndex), n, 1);
  elseif isstring(template)
    labels = reshape(template(classIndex), n, 1);
  elseif iscategorical(template)
    labels = reshape(template(classIndex), n, 1);
  else
    labels = reshape(template(classIndex), n, 1);
  end
end
%=============================================================================
