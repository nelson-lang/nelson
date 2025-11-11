%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bdschur(varargin)
  % Block-diagonal Schur factorization
  % [T, B] = bdschur(A)
  % [T, B] = bdschur(A, CONDMAX)
  
  narginchk(1, 2);
  nargoutchk(0, 2);
  
  A = varargin{1};
  mustBeReal(A, 1);
  mustBeFinite(A, 1);
  
  if nargin > 1
    CONDMAX = varargout{2};
  else
    CONDMAX = 1e4;
  end
  mustBeReal(CONDMAX, 2);
  mustBeScalarOrEmpty(CONDMAX, 2);
  
  if isempty(A)
    T = A;
    B = A;
    varargout{1} = T;
    if nargout > 1
      varargout{2} = B;
    end
    return;
  end
  
  [rA, cA] = size(A);
  if (rA ~= cA)
    error(_('Wrong size for input argument #1: square matrix expected.'));
  end
  
  JOBX =  'U';
  SORT = 'S';
  PMAX = CONDMAX;
  [U, ~] = schur(A);
  TOL = 0; % default lapack tolerance
  [A_OUT, X_OUT, NBLCKS, BLSIZE, WR, WI, INFO] = slicot_mb03rd(JOBX, SORT, CONDMAX, A, U, TOL);
  
  if (INFO(1) ~= 0) 
    error(sprintf( _('slicot_mb03rd returned info = %d'), INFO(1)));
  end
  
  T = X_OUT;
  B = A_OUT;
  varargout{1} = T;
  if nargout > 1
    varargout{2} = B;
  end
end
