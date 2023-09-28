%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: MIT OR LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function msg = checkABCDE(varargin)
  narginchk(4, 5);
  if (nargin == 5)
    E = varargin{5};
  else
    E = [];
  end
  A = varargin{1};
  B = varargin{2};
  C = varargin{3};
  D = varargin{4};
  
  [mA, nA] = size(A);
  [mB, nB] = size(B);
  [mC, nC] = size(C);
  [mD, nD] = size(D);
  [mE, nE] = size(E);
  
  msg.message = '';
  msg.identifier = '';
  msg = msg(zeros(0, 1));
  
  if (mA ~= nA)
    msg.message = _('Matrix A must be square.');
    msg.identifier = 'Nelson:control_system:AMustBeSquare';
    return
  end
  
  if (mB ~= mA)
    msg.message = _('The number of rows in matrices A and B must be equal.');
    msg.identifier = 'Nelson:control_system:AAndBNumRowsMismatch';
    return
  end
  
  if (nC ~= nA)
    msg.message = _('Matrices A and C should have an identical number of columns.');
    msg.identifier = 'Nelson:control_system:AAndCNumColumnsMismatch';
    return
  end
  
  if (nB ~= nD)
    msg.message = _('Matrices B and D should have an identical number of columns.');
    msg.identifier = 'Nelson:control_system:BAndDNumColumnsMismatch';
    return
  end
  
  if (mC ~= mD)
    msg.message = _('The number of rows in matrices C and D must be equal.');
    msg.identifier = 'Nelson:control_system:CAndDNumRowsMismatch';
    return
  end
  
  if ~isempty(E) && (mA ~= mE || nA ~= nE)
    msg.message = _('Matrices A and E should have an identical size.');
    msg.identifier = 'Nelson:control_system:AAndESizeMismatch';
  end
end
%=============================================================================
