%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = kron(varargin)
  % https://en.wikipedia.org/wiki/Kronecker_product
  narginchk(2, 2);
  nargoutchk(0, 1);
  
  A = varargin{1};
  B = varargin{2};
  
  if ~ismatrix(A) || ~ismatrix(B)
    error(_('Inputs must be 2-D.'));
  end
  
  % need to be optimized ...
  if issparse(A) || issparse(B)
    varargout{1} = sparse(kron(full(A), full(B)));
  else
    [m, n] = size(A);
    [p, q] = size(B);
    result = zeros(m * p, n * q);
    for i = 1:m
      for j = 1:n
        a_ij = A(i, j);
        submatrix = a_ij * B;
        row_start = (i - 1) * p + 1;
        row_end = i * p;
        col_start = (j - 1) * q + 1;
        col_end = j * q;
        result(row_start:row_end, col_start:col_end) = submatrix;
      end
    end    
    varargout{1} = result;
  end
end
%=============================================================================
