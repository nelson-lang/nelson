%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function yfit = predict(obj, X)
  narginchk(2, 2);
  nargoutchk(0, 1);
  X = validatePredictorsLocal(X, obj.NumPredictors);
  Xq = standardizeLocal(double(X), obj);
  K = kernelMatrixLocal(Xq, obj.SupportVectors, obj);
  yfit = K * obj.Alpha + obj.Bias;
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
