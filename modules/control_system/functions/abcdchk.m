%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = abcdchk(varargin)
  narginchk(0, 5);
  nargoutchk(0, 6);
  
  if nargin < 5, E = []; else E = varargin{5}; end
  if nargin < 4, D = []; else D = varargin{4}; end
  if nargin < 3, C = []; else C = varargin{3}; end
  if nargin < 2, B = []; else B = varargin{2}; end
  if nargin < 1, A = []; else A = varargin{1}; end
  
  [ME, NE] = size(E);
  E_MODIFIED = E;
  [MD, ND] = size(D);
  D_MODIFIED = D;
  [MC, NC] = size(C);
  C_MODIFIED = C; 
  [MB, NB] = size(B); 
  B_MODIFIED = B;
  [MA, NA] = size(A);
  A_MODIFIED = A;
  if (MC == 0) && (NC == 0) && (MD == 0 || NA == 0)
    MC = MD;
    NC = NA;
    C_MODIFIED = zeros(MC, NC, 'like', C);
  end
  if (MB == 0) && (NB == 0) && (MA == 0 || ND == 0)
    MB = MA; 
    NB = ND; 
    B_MODIFIED = zeros(MB, NB,'like', B);
  end
  if (MD == 0) && (ND == 0) && (MC == 0 || NB == 0)
    MD = MC;
    ND = NB; 
    D_MODIFIED = zeros(MD, ND, 'like', D);
  end
  if (MA == 0) && (NA == 0) && (MB == 0 || NC == 0)
    MA = MB;
    NA = NC; 
    A_MODIFIED = zeros(MA, NA, 'like', A);
  end
  
  msg.message = '';
  msg.identifier = '';
  msg = msg(zeros(0, 1));
  if (MA ~= NA) && (nargin >= 1)
    msg.message = 'Nelson:control_system:AMustBeSquare';
    msg.identifier = _('Matrix A must be square.');
  elseif (MA ~= MB) && (nargin >= 2)
    msg.message = 'Nelson:control_system:AAndBNumRowsMismatch';
    msg.identifier = _('The number of rows in matrices A and B must be equal.');
  elseif (NA ~= NC) && (nargin >= 3)
    msg.message = _('Matrices A and C should have an identical number of columns.');
    msg.identifier = 'Nelson:control_system:AAndCNumColumnsMismatch';
  elseif (MD ~= MC) && (nargin >= 4)
    msg.message = _('The number of rows in matrices C and D must be equal.');
    msg.identifier = 'Nelson:control_system:CAndDNumRowsMismatch';
  elseif (ND ~= NB) && (nargin >= 4)
    msg.message = _('Matrices B and D should have an identical number of columns.');
    msg.identifier = 'Nelson:control_system:BAndDNumColumnsMismatch';
  end
  
  if (nargin > 4)
    if (~((MA == ME) && (NA == NE)) && isempty(E))
      msg = _('Matrices A and E should have an identical size.');
      msg.identifier = 'Nelson:control_system:AAndESizeMismatch';
    end
  else
    ME = MA;
    NE = NA; 
    E_MODIFIED = zeros(ME, NE, 'like', A);
  end
  
  varargout{1} = msg;
  if nargout > 1
    varargout{2} = A_MODIFIED;
  end
  if nargout > 2
    varargout{3} = B_MODIFIED;
  end
  if nargout > 3
    varargout{4} = C_MODIFIED;
  end
  if nargout > 4
    varargout{5} = D_MODIFIED;
  end
  if nargout > 5
    varargout{6} = E_MODIFIED;
  end
  
end
%=============================================================================
