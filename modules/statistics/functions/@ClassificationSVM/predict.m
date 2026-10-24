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
  Xq = standardizeLocal(double(X), obj);
  decision = decisionFunctionLocal(obj, Xq);
  classIndex = ones(size(decision));
  classIndex(decision >= 0) = 2;
  label = labelsFromIndexLocal(obj, classIndex, size(X, 1));
  score = [-decision decision];
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
function decision = decisionFunctionLocal(obj, Xq)
  if isempty(obj.SupportVectors)
    decision = obj.Bias * ones(size(Xq, 1), 1);
    return
  end
  K = kernelMatrixLocal(Xq, obj.SupportVectors, obj);
  decision = K * (obj.Alpha .* obj.SupportVectorLabels) + obj.Bias;
end
%=============================================================================
function K = kernelMatrixLocal(A, B, obj)
  switch obj.KernelFunction
    case 'gaussian'
      a2 = sum(A .^ 2, 2);
      b2 = sum(B .^ 2, 2).';
      dist2 = repmat(a2, 1, size(B, 1)) + repmat(b2, size(A, 1), 1) - 2 * (A * B.');
      K = exp(-max(dist2, 0) / (2 * obj.KernelScale ^ 2));
    case 'polynomial'
      K = (1 + A * B.') .^ obj.PolynomialOrder;
    otherwise
      K = A * B.';
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
