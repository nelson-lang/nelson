%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [label, score] = predict(obj, X)
  narginchk(2, 2);
  nargoutchk(0, 2);
  X = validatePredictorsLocal(X, obj.NumPredictors);
  score = aggregateScoresLocal(obj, X);
  [~, classIndex] = max(score, [], 2);
  label = labelsFromIndexLocal(obj.ClassNames, classIndex, size(X, 1));
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
function score = aggregateScoresLocal(obj, X)
  n = size(X, 1);
  k = length(obj.ClassNames);
  score = zeros(n, k);
  for m = 1:length(obj.BinaryLearners)
    learner = obj.BinaryLearners{m};
    labels = predict(learner, X);
    classIndex = labelIndexLocal(obj.ClassNames, labels);
    for row = 1:n
      score(row, classIndex(row)) = score(row, classIndex(row)) + 1;
    end
  end
  score = score / max(length(obj.BinaryLearners), 1);
end
%=============================================================================
function index = labelIndexLocal(classNames, labels)
  index = zeros(length(labels), 1);
  for row = 1:length(labels)
    for k = 1:length(classNames)
      if isequal(labelAtLocal(labels, row), labelAtLocal(classNames, k))
        index(row) = k;
        break
      end
    end
  end
end
%=============================================================================
function value = labelAtLocal(labels, index)
  if iscell(labels)
    value = labels{index};
  else
    value = labels(index);
  end
end
%=============================================================================
function labels = labelsFromIndexLocal(classNames, classIndex, n)
  if isnumeric(classNames)
    labels = reshape(classNames(classIndex), n, 1);
  elseif islogical(classNames)
    labels = reshape(classNames(classIndex), n, 1);
  elseif isstring(classNames)
    labels = reshape(classNames(classIndex), n, 1);
  elseif iscategorical(classNames)
    labels = reshape(classNames(classIndex), n, 1);
  else
    labels = reshape(classNames(classIndex), n, 1);
  end
end
%=============================================================================
