%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
    msg.message = sprintf(_('%s must be square.'), 'Matrix A');
    msg.identifier = 'Nelson:expectedSquare';
    return
  end

  if (mB ~= mA)
    msg.message = sprintf(_('The number of rows in matrices %s and %s must be equal.'), 'A', 'B');
    msg.identifier = 'Nelson:control_system:AAndBNumRowsMismatch';
    return
  end

  if (nC ~= nA)
    msg.message = sprintf(_('Matrices %s and %s should have an identical number of columns.'), 'A', 'C');
    msg.identifier = 'Nelson:control_system:AAndCNumColumnsMismatch';
    return
  end

  if (nB ~= nD)
    msg.message = sprintf(_('Matrices %s and %s should have an identical number of columns.'), 'B', 'D');
    msg.identifier = 'Nelson:control_system:BAndDNumColumnsMismatch';
    return
  end

  if (mC ~= mD)
    msg.message = sprintf(_('The number of rows in matrices %s and %s must be equal.'), 'C', 'D');
    msg.identifier = 'Nelson:control_system:CAndDNumRowsMismatch';
    return
  end

  if ~isempty(E) && (mA ~= mE || nA ~= nE)
    msg.message = sprintf(_('Matrices %s and %s should have an identical size.'), 'A', 'E');
    msg.identifier = 'Nelson:control_system:AAndESizeMismatch';
  end
end
%=============================================================================
