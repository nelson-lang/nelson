%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = poly(r)
  narginchk(1, 1);
  nargoutchk(0, 1);
  rowCount = size(r, 1);
  columnCount = size(r, 2);
  if rowCount == columnCount
    rootValues = eig(r);
  elseif ((rowCount == 1) || (columnCount == 1))
    rootValues = r;
  else
    error('Nelson:poly:InputSize', 'Argument must be a vector or a square matrix.')
  end
  rootValues = rootValues(isfinite(rootValues));
  rootCount = length(rootValues);
  p = zeros(1, rootCount + 1, class(r));
  p(1) = 1;
  for rootIndex = 1:rootCount
    activeRange = 1:rootIndex;
    p(activeRange + 1) = p(activeRange + 1) - rootValues(rootIndex) .* p(activeRange);
  end
  positiveImaginaryRoots = sort(rootValues(imag(rootValues) > 0));
  negativeImaginaryRoots = sort(conj(rootValues(imag(rootValues) < 0)));
  if isequal(positiveImaginaryRoots, negativeImaginaryRoots)
    p = real(p);
  end
end
%=============================================================================
