%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = rot90(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  
  A = varargin{1};
  k = 1;
  if nargin == 2
    k = varargin{2};
    if ~isscalar(k)
      error('Nelson:rot90:kNonScalar', _('Input argument #1: scalar expected.'));
    end
    k = mod(k, 4);
  end
  if (~issparse(A) && ismatrix(A) && (isnumeric(A) || islogical(A)))
    varargout{1} = __rot90_matrix_2D__(A, k);
    return
  end
  
  switch k
    case 0
      B = A;
    case 1
      B = flip(A, 2);
      B = permute(B, [2, 1, 3:ndims(A)]);
    case 2
      B = flip(flip(A, 1),2);
    case 3
      B = permute(A,[2, 1, 3:ndims(A)]);
      B = flip(B, 2);
    otherwise
      error('Nelson:rot90:kNonInteger', _('Input argument #1: scalar integer value expected.'));
    end
    varargout{1} = B;
  end
  %=============================================================================