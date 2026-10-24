%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [label, score, cost, node] = predict(obj, X)
  narginchk(2, 2);
  nargoutchk(0, 4);
  X = validatePredictorsLocal(X, obj.NumPredictors);
  node = terminalNodesLocal(double(X), obj);
  score = obj.NodeProbability(node, :);
  [~, classIndex] = max(score, [], 2);
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
function node = terminalNodesLocal(X, obj)
  % Every row descends the tree independently, so they descend together: one
  % vectorised step per LEVEL instead of one interpreted loop per row. The
  % properties are read once here rather than once per node visited, which
  % is where most of the old cost went.
  isBranch = obj.IsBranch(:);
  cutPredictor = obj.CutPredictor(:);
  cutPoint = obj.CutPoint(:);
  leftChild = obj.LeftChild(:);
  rightChild = obj.RightChild(:);
  count = size(X, 1);
  node = ones(count, 1);
  if count == 0
    return;
  end
  moving = find(isBranch(node));
  while ~isempty(moving)
    here = node(moving);
    % The column each of these rows is tested on differs from row to row, so
    % the values are gathered by linear index rather than by a column slice.
    values = X(moving + (cutPredictor(here) - 1) * count);
    goesLeft = values <= cutPoint(here);
    node(moving(goesLeft)) = leftChild(here(goesLeft));
    node(moving(~goesLeft)) = rightChild(here(~goesLeft));
    moving = moving(isBranch(node(moving)));
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
