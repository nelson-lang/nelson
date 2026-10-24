%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [X, yIndex, classNames, Yclean] = __classification_training_data__(X, Y, varargin)
  exactClasses = [];
  if nargin > 2
    exactClasses = varargin{1};
  end
  if ~(isnumeric(X) && isreal(X) && ismatrix(X) && ~issparse(X))
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  X = double(X);
  if isempty(X) || size(X, 2) == 0
    error('Nelson:statistics:xObservationAndPredictor', 'X must contain at least one observation and one predictor.');
  end
  if any(~isfinite(X(:)))
    error('Nelson:statistics:xMustBeFinite', '%s must contain finite values.', 'X');
  end
  if ~isvector(Y) || length(Y) ~= size(X, 1)
    error('Nelson:statistics:yVectorOneLabelRow', 'Y must be a vector with one %s per row of X.', 'label');
  end
  [yIndex, ~, classNames] = grp2idx(Y(:));
  yIndex = double(yIndex(:));
  keep = ~isnan(yIndex);
  X = X(keep, :);
  yIndex = yIndex(keep);
  if isempty(yIndex) || length(unique(yIndex)) < 2
    error('Nelson:statistics:yContainLeastTwoClasses', 'Y must contain at least two classes.');
  end
  if ~isempty(exactClasses) && length(unique(yIndex)) ~= exactClasses
    error('Nelson:statistics:yContainExactlyTwoClasses', 'Y must contain exactly two classes.');
  end
  if nargout > 3
    Yclean = __labels_from_class_index__(classNames, yIndex, length(yIndex));
  end
end
%=============================================================================
