%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [X, y] = __regression_training_data__(X, Y)
  if ~(isnumeric(X) && isreal(X) && ismatrix(X) && ~issparse(X))
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if ~(isnumeric(Y) && isreal(Y) && isvector(Y) && ~issparse(Y))
    error('Nelson:statistics:yRealNumericVector', '%s must be a real numeric vector.', 'Y');
  end
  X = double(X);
  y = double(Y(:));
  if isempty(X) || size(X, 2) == 0
    error('Nelson:statistics:xObservationAndPredictor', 'X must contain at least one observation and one predictor.');
  end
  if length(y) ~= size(X, 1)
    error('Nelson:statistics:yVectorOneResponseRow', 'Y must be a vector with one %s per row of X.', 'response');
  end
  if any(isinf(X(:))) || any(isinf(y))
    error('Nelson:statistics:xYFiniteOrNaN', 'X and Y must contain finite values or NaN missing values.');
  end
  keep = ~isnan(y) & all(~isnan(X), 2);
  X = X(keep, :);
  y = y(keep);
  if isempty(y)
    error('Nelson:statistics:validObservationsRemainAfterRemoving', 'No valid observations remain after removing missing values.');
  end
end
%=============================================================================
