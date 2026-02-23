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
function varargout = dlyap(varargin)
  % Compute solution X of the Lyapunov equation
  narginchk(2, 2);
  nargoutchk(0, 1);
  
  A = varargin{1};
  Q = varargin{2};
  
  mustBeNumeric(A, 1);
  mustBeNumeric(Q, 2);
  mustBeReal(A, 1);
  mustBeReal(Q, 2);

  if ~issquare(A)
    error(message('nelson:validators:mustBeSquareMatrixAtPosition', 1));
  end
  if ~issquare(Q)
    error(message('nelson:validators:mustBeSquareMatrixAtPosition', 2));
  end
  p = kron(conj(A), A);
  K = eye(size(p)) - p;
  X = K \ Q(:);
  varargout{1} = reshape(X, size(A));
end
%=============================================================================
