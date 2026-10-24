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
  logScore = gaussianLogPosteriorLocal(double(X), obj);
  score = normalizeLogScoresLocal(logScore);
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
function logScore = gaussianLogPosteriorLocal(X, obj)
  n = size(X, 1);
  kClasses = length(obj.ClassNames);
  logScore = zeros(n, kClasses);
  for j = 1:kClasses
    sigma = max(obj.Sigma(j, :), realmin);
    centered = X - repmat(obj.Mu(j, :), n, 1);
    logDensity = -0.5 * sum((centered ./ repmat(sigma, n, 1)) .^ 2, 2) ...
      - sum(log(sigma)) - 0.5 * obj.NumPredictors * log(2 * pi);
    logScore(:, j) = log(max(obj.Prior(j), realmin)) + logDensity;
  end
end
%=============================================================================
function score = normalizeLogScoresLocal(logScore)
  rowMax = max(logScore, [], 2);
  shifted = exp(logScore - repmat(rowMax, 1, size(logScore, 2)));
  denom = sum(shifted, 2);
  score = shifted ./ repmat(denom, 1, size(shifted, 2));
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
