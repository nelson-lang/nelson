%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
  if islogical(A) || ischar(A)
    A = double(A);
  end
  if islogical(B) || ischar(B)
    B = double(B);
  end

  if ~ismatrix(A) || ~ismatrix(B)
    error('Nelson:kron:inputMustBe2D', 'Inputs must be 2-D.');
  end

  if issparse(A) || issparse(B)
    varargout{1} = sparse(kron(full(A), full(B)));
  elseif (isa(A, 'double') || isa(A, 'single')) && ...
      (isa(B, 'double') || isa(B, 'single')) && ...
      isreal(A) && isreal(B)
    varargout{1} = builtin('kron', A, B);
  else
    [m, n] = size(A);
    [p, q] = size(B);
    % preserve integer / single classes (aligned with expected behavior)
    if isinteger(A)
      outClass = class(A);
    elseif isinteger(B)
      outClass = class(B);
    elseif isa(A, 'single') || isa(B, 'single')
      outClass = 'single';
    else
      outClass = 'double';
    end
    result = zeros(m * p, n * q, outClass);
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
