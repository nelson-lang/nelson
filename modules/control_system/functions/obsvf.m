%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = obsvf(varargin)
  % Compute observability staircase form
  % [Abar, Bbar, Cbar, T, k] = obsvf(A, B, C)
  % [Abar, Bbar, Cbar, T, k] = obsvf(A, B, C, tol)
  
  narginchk(3, 4);
  nargoutchk(0, 5);
  
  A = varargin{1};
  B = varargin{2};
  C = varargin{3};
  
  if nargin == 3
    [AA, BB, CC, T, K] = ctrbf(A', C', B');
  else
    tol = varargin{4};
    [AA, BB, CC, T, K] = ctrbf(A', C', B', tol);
  end
  
  varargout{1} = AA';
  varargout{2} = CC';
  varargout{3} = BB';
  varargout{4} = T;
  varargout{5} = K;
end
%=============================================================================
