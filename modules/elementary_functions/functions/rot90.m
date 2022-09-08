%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
      error(message('MATLAB:rot90:kNonScalar'));
    end
    k = mod(k, 4);
  end
  switch k
    case 0
      B = A;
    case 1
      B = flip(A, 2);
      B = permute(B, [2 1 3:ndims(A)]);
    case 2
      B = flip(flip(A, 1),2);
    case 3
      B = permute(A,[2 1 3:ndims(A)]);
      B = flip(B, 2);
    otherwise
      error(message('MATLAB:rot90:kNonInteger'));
    end
    varargout{1} = B;
  end
  %=============================================================================