%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = condeig(varargin)
  narginchk(1, 1);
  nargoutchk(0, 3);
  A = varargin{1};
  
  if (~(isnumeric(A) && size(A, 1) == size(A, 2)))
    error (_('A must be a square numeric matrix.'));
  end
  
  [V, L] = eig(A);
  
  if ~isempty(A)
    VL = inv(V);
    VL = VL';
    rows = size(VL, 1);
    VL = VL ./ repmat (sqrt (sum (abs(VL .^ 2))), rows, 1);
    C = abs (1 ./ dot(VL, V)');
  else
    C = [];
  end
  
  if nargout < 2
    varargout{1} = C;
  else
    varargout{1} = V;
    varargout{2} = L;
    varargout{3} = C;
  end
end
%=============================================================================
