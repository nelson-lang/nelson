%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [yfit, node] = predict(obj, X)
  narginchk(2, 2);
  nargoutchk(0, 2);
  X = validatePredictorsLocal(X, obj.NumPredictors);
  node = terminalNodesLocal(double(X), obj);
  yfit = obj.NodeMean(node);
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
function node = terminalNodesLocal(X, obj)
  node = ones(size(X, 1), 1);
  for i = 1:size(X, 1)
    current = 1;
    while obj.IsBranch(current)
      predictor = obj.CutPredictor(current);
      if X(i, predictor) <= obj.CutPoint(current)
        current = obj.LeftChild(current);
      else
        current = obj.RightChild(current);
      end
    end
    node(i) = current;
  end
end
%=============================================================================
