%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ctrb(varargin)
  % Generates the controllability matrix of a state space model
  % Co = ctrb(A,B)
  % Co = ctrb(sys)
  narginchk(1, 2);
  nargoutchk(0, 1);
  
  if nargin == 1
    sys = varargin{1};
    sys = ss(sys);
    A = sys.A;
    B = sys.B;
  else
    A = varargin{1};
    B = varargin{2};
    [mA, nA] = size(A);
    [mB, nB] = size(B);
    if (mA ~= nA)
      error(message('nelson:control_system:AMustBeSquare'));
    end
    
    if (mB ~= mA)
      error(message('nelson:control_system:AAndBNumRowsMismatch'));
    end
  end        
  
  n = size(A, 1); % We only check the dimension of A
  % Compute the controllability matrix now!
  Cs = [];
  for i = 0:(n-1)
    Cs = [Cs, A^i*B];
  end
  varargout{1} = Cs;
end
%=============================================================================
