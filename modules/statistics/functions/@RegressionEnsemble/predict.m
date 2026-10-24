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
  if strcmpi(obj.Method, 'LSBoost')
    yfit = ensembleBoostLocal(obj, X);
  else
    yfit = ensembleMeanLocal(obj, X);
  end
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
function yfit = ensembleMeanLocal(obj, X)
  yfit = zeros(size(X, 1), 1);
  for m = 1:length(obj.Learners)
    yfit = yfit + predict(obj.Learners{m}, X);
  end
  yfit = yfit / max(length(obj.Learners), 1);
end
%=============================================================================
function yfit = ensembleBoostLocal(obj, X)
  yfit = obj.InitialResponse * ones(size(X, 1), 1);
  weights = obj.LearnerWeights;
  if isempty(weights)
    weights = obj.LearnRate * ones(1, length(obj.Learners));
  end
  for m = 1:length(obj.Learners)
    yfit = yfit + weights(m) * predict(obj.Learners{m}, X);
  end
end
%=============================================================================
