%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d2 = mahal(Y, X)
  narginchk(2, 2);
  nargoutchk(0, 1);
  [Y, X, castSingle] = validateInputsLocal(Y, X);
  if size(Y, 1) == 0
    d2 = zeros(0, 1);
    if castSingle
      d2 = single(d2);
    end
    return
  end
  center = mean(X, 1);
  C = cov(X);
  D = Y - repmat(center, size(Y, 1), 1);
  solved = D / C;
  d2 = sum(solved .* D, 2);
  if castSingle
    d2 = single(d2);
  end
end
%=============================================================================
function [Y, X, castSingle] = validateInputsLocal(Y, X)
  if ~(isnumeric(Y) && isreal(Y) && ~issparse(Y) && ndims(Y) == 2)
    error('Nelson:statistics:yRealNumericMatrix', '%s must be a real numeric matrix.', 'Y');
  end
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if isempty(X)
    error('Nelson:statistics:xYNonemptyMatrices', 'X and Y must be nonempty matrices.');
  end
  if size(Y, 2) ~= size(X, 2)
    error('Nelson:statistics:xyColumnMismatch', 'X and Y must have the same number of columns.');
  end
  if size(X, 1) <= size(X, 2)
    error('Nelson:statistics:xMoreRowsColumns', 'X must have more rows than columns.');
  end
  castSingle = isa(Y, 'single') && isa(X, 'single');
  Y = double(Y);
  X = double(X);
end
%=============================================================================
