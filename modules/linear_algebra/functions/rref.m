%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = rref (varargin)
  narginchk(1, 2);
  nargoutchk(0, 2);
  A = varargin{1};
  if (ndims (A) > 2)
    error (_('Inputs must be 2-D.'));
  end
  [rows, cols] = size (A);
  
  if (nargin == 2)
    tol = varargin{2};
  else
    tol = max(rows, cols) * eps(class(A)) * norm(A, inf);
  end
  
  if isempty(A)
    varargout{1} = A;
    if nargout > 1
      varargout{2} = zeros(1, 0);
    end
    return
  end
  used = zeros (1, cols);
  r = 1;
  for c = 1:cols
    [m, p] = max(abs(A(r:rows,c)));
    if (m > tol)
      used(1, c) = 1;
      ccols = c:cols;
      A(r, ccols) = A(r, ccols) / A(r, c);
      ridx = [1:r - 1, r + 1:rows];
      A(ridx, ccols) = A(ridx, ccols) - A(ridx, c) * A(r, ccols);
      if (r == rows)
        break;
      end
      r = r + 1;
    else
      A(r:rows, c) = zeros(rows - r + 1, 1);
    end
  end
  varargout{1} = A;
  if nargout > 1
    varargout{2} = find(used);
  end
end
%=============================================================================
