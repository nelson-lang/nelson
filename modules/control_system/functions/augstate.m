%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = augstate(varargin)
  narginchk(4, 4);
  nargoutchk(0, 4);
  
  A = varargin{1};
  B = varargin{2};
  C = varargin{3};
  D = varargin{4};
  
  msg = abcdchk(A, B);
  if ~isempty(msg)
    error(msg.id, msg.message);
  end
  
  rA = size(A, 1);
  cB = size(B, 2);
  
  varargout{1} = A; 
  if nargout > 1
    varargout{2} = B;
  end
  if nargout > 2
    varargout{3} = [C; eye(rA)];
  end
  if nargout > 2
    varargout{4} = [D; zeros(rA, cB)];
  end
end
