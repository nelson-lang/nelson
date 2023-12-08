%=============================================================================
% Copyright (c) 2017 Oktober Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = obsv(varargin)
  % Generates the observability matrix of a state space model
  % Ob = obsv(A, C)
  % Ob = obsv(sys)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if nargin == 1
    sys = varargin{1};
    sys = ss(sys);
    A = sys.A;
    C = sys.C;
  else
    A = varargin{1};
    C = varargin{2};
    [mA, nA] = size(A);
    [mC, nC] = size(C);
    if (mA ~= nA)
      error('Nelson:control_system:AMustBeSquare', _('Matrix A must be square.'));
    end
    if (nC ~= nA)
      error('Nelson:control_system:AAndCNumColumnsMismatch', _('Matrices A and C should have an identical number of columns.'));
    end
  end        
  n = size(A, 1); 
  % Compute the observability matrix now!
  Or = [];
  for i = 0:(n-1)
    Or = [Or; C * A^i];
  end
  varargout{1} = Or; 
end
