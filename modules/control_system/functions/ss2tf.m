%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ss2tf(varargin)
  % Transform state space model to transfer function
  % [num, den] = ss2tf(A, B, C, D)
  % [num, den] = ss2tf(A, B, C, D, iu)
  
  narginchk(4, 5);
  nargoutchk(0, 2);
  
  % Extract inputs
  A = varargin{1};
  B = varargin{2};
  C = varargin{3};
  D = varargin{4};
  
  % Validate state-space matrices
  msg = checkABCDE(A, B, C, D);
  if ~isempty(msg)
    error(msg.message, msg.identifier);
  end
  
  nu = size(B, 2);
  
  % Validate and handle input index (iu)
  if nargin == 4
    if nu <= 1
      iu = 1;
    else
      error(_('Single input system expected.'));
    end
  else
    iu = varargin{5};
    validateInputIndex(iu, nu);
  end
  
  % Compute transfer function coefficients
  [numerator, denominator] = computeTransferFunctionCoefficients(A, B, C, D, iu);
  
  % Only real parts for coefficients are desired
  numerator = real(numerator);
  denominator = real(denominator);
  
  % Output results
  varargout{1} = removeLeadingZeroColumns(numerator);
  if nargout > 1
    varargout{2} = denominator;
  end
end
%=============================================================================
function validateInputIndex(iu, nu)
  % Validate input index (iu)
  if ~isnumeric(iu) || ~isscalar(iu) || iu < 1 || iu > nu
    error(_('Index out of bounds or not a scalar.'));
  end
end
%=============================================================================
function [numerator, denominator] = computeTransferFunctionCoefficients(A, B, C, D, iu)
  % Compute transfer function coefficients
  denominator = poly(A);
  
  if ~isempty(B) && iu >= 1 && iu <= size(B, 2)
    numerator = computeNumerator(A, B, C, D, denominator, iu);
  else
    % System is a gain or has only a denominator
    numerator = D;
    if isempty(D) && isempty(A)
      denominator = [];
    end
  end
end
%=============================================================================
function p = removeLeadingZeroColumns(p)
  [n, m] = size(p);
  % Check if there are multiple columns and the norm of the first column is zero
  if m > 1 && norm(p(:, 1)) == 0
    % Remove the first column and recursively call removeLeadingZeroColumns
    p(:, 1) = [];
    p = removeLeadingZeroColumns(p);
  end
end
%=============================================================================
function V = computeNumerator(A, B, C, D, denominator, iu)
  mc = size(D, 1);
  nc = length(A);
  V = zeros(mc, nc + 1);
  
  for k = 1:mc
    p = A - B(:, iu) * C(k, :);
    V(k, :) = poly(p) - poly(A) + D(k, iu) * denominator;
  end
  
  if isvector(V)
    idx = find(V);
    if ~isempty(idx)
      V = V(idx(1):end);
    end
  end
end
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
