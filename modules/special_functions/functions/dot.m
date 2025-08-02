%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = dot(varargin)
  % dot product
  narginchk(2, 3);
  nargoutchk(0, 1);
  A = varargin{1};
  B = varargin{2};
  
  if (nargin == 3)
    dim = varargin{3};
    if ~isnumeric(dim) || ~isscalar(dim)
      error(_('Dimension argument must be a positive integer scalar.'));
    end
  end
  if isinteger(A) || isinteger(B)
    error(_('A and B must be single or double.'));
  end
  
  if nargin ~= 2
    if ~isequal(size(A), size(B))
      error(_('A and B must be same size.'));
    end
    if  isreal(A) && isreal(B)
      if iscolumn(A) && dim == 1
        varargout{1} = A' * B;
        return
      elseif isrow(A) && dim == 2
        varargout{1} = A * B';
        return
      end
    end
    varargout{1} = sum(conj(A) .* B, dim);
  else
    if isvector(A) && isvector(B)
      A = A(:);
      B = B(:);
      if (length(A) ~= length(A))
        error(_('A and B must be same size.'));
      end
      if isreal(A) && isreal(B)
        varargout{1} = A' * B;
        return
      end
    elseif ~isequal(size(A), size(B))
      error(_('A and B must be same size.'));
    end
    varargout{1} = sum(conj(A) .* B);
  end
end
%=============================================================================
