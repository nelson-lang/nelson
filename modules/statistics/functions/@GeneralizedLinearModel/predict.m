%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function yfit = predict(obj, Xnew)
  narginchk(2, 2);
  nargoutchk(0, 1);
  Xnew = validatePredictorsLocal(Xnew, obj.NumPredictors);
  D = designMatrixLocal(double(Xnew), obj.Terms, obj.Intercept);
  yfit = inverseLinkLocal(D * obj.Beta, obj.Link);
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
function D = designMatrixLocal(X, terms, intercept)
  D = zeros(size(X, 1), 0);
  if intercept
    D = ones(size(X, 1), 1);
  end
  for k = 1:size(terms, 1)
    col = ones(size(X, 1), 1);
    for j = 1:size(X, 2)
      if terms(k, j) ~= 0
        col = col .* (X(:, j) .^ terms(k, j));
      end
    end
    D = [D col];
  end
end
%=============================================================================
function mu = inverseLinkLocal(eta, link)
  switch link
    case 'log'
      mu = exp(eta);
    case 'logit'
      mu = 1 ./ (1 + exp(-eta));
    otherwise
      mu = eta;
  end
end
%=============================================================================
