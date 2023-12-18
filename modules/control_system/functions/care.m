%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = care(varargin)
  % Solves the Continuous-time Algebraic Riccati Equation
  % [X, L, G] = care(A, B, Q)
  % [X, L, G] = care(A, B, Q, R)
  % [X, L, G] = care(A, B, Q, R, S, E)
  
  narginchk(3, 6);
  nargoutchk(0, 3);
  
  A = varargin{1};
  B = varargin{2};
  Q = varargin{3};
  mustBeNumeric(A, 1);
  mustBeNumeric(B, 2);
  mustBeNumeric(Q, 3);
  
  [m, n] = size(B);
  
  R = [];
  S = [];
  E = [];
  
  if nargin > 3
    R = varargin{4};
    mustBeNumeric(R, 4);
    if ~issymmetric(R, 0)
      error(_('Wrong value for argument #4: Symmetric matrix expected.'));
    end
  else
    R = eye(n, n);
  end
  if nargin > 4
    S = varargin{5};
  else
    S = zeros(m, n);
  end
  if nargin > 5
    E = varargin{6};
  else
    E = eye(m, m);
  end
  mustBeNumeric(S, 5);
  mustBeNumeric(E, 6);
  
  [rA, cA] = size(A);
  if (rA ~= m) || (cA ~= m)
    error(_('Wrong size for argument #1: Square matrix expected.'));
  end
  
  if ((size(Q, 1) ~= size(Q, 2)) || (size(R, 1) ~= size(R, 2)))
    error(_('Wrong size for arguments: Q, R square matrix expected.'));
  end
  
  if (size(A, 1) ~= m)
    error(_('Wrong size for arguments #1 and #2: Same number of rows expected.'));
  end
  
  if (size(R, 2) ~= n)
    error(_('Wrong size for arguments #2 and #4: Same number of columns expected.'));
  end
  
  if ~isempty(S) && ((size(S,1) ~= m) || (size(S,2) ~= n))
    error(_('Wrong size for arguments #2 and #5: Same size expected.'));
  end
  
  if ~isempty(E) && ((size(E,1) ~= size(A,1)) || (size(E,2) ~= size(A,2)))
    error(_('Wrong size for arguments #1 and #6: Same number of rows expected.'));
  end
  
  if (size(A, 1) ~= size(Q, 1)) && (size(A, 2) ~= size(Q, 2))
    error(_('Wrong size for arguments #1 and #3: Same size expected.'));
  end
  
  mustBeFinite(A, 1);
  mustBeFinite(B, 2);
  mustBeFinite(Q, 3);
  mustBeFinite(R, 4);
  
  X = [];
  L = [];
  G = [];
  
  DICO = 'C';
  JOBB = 'B';
  FACT = 'N';
  UPLO = 'U';
  SORT = 'S';
  P = 0;
  TOL = 0;
  
  JOBL = 'N';
  SorB = S;
  if isempty(S)
    JOBL = 'Z';
    SorB = B;
  end
  
  if isempty(E)
    [RCOND, X, ALFAR, ALFAI, BETA, S_OUT, T, U, INFO] = slicot_sb02od(DICO, JOBB, FACT, UPLO, JOBL, SORT, P, A, B, Q, R, SorB, TOL);
    if (INFO(1) ~= 0)
      error(_('Unable to solve the specified Riccati equation.'));
    end
  else
    SCAL = 'N';
    ACC = 'N';
    [RCONDU, X, ALFAR, ALFAI, BETA, S_OUT, T, U, IWARN, INFO] = slicot_sg02ad(DICO, JOBB, FACT, UPLO, JOBL, SCAL, SORT, ACC, P, A, E, B, Q, R, SorB, TOL);
    if (INFO(1) ~= 0)
      error(_('Unable to solve the specified Riccati equation.'));
    end
    if (IWARN(1) == 1) 
      warning(_("The accuracy of the solution could be compromised by inadequate scaling or eigenvalues that are in close proximity to the stability domain's boundary."));
    end
  end
  N = size(A, 1);
  ALFAR = ALFAR([1:N]);
  ALFAI = ALFAI([1:N]);
  BETA = BETA([1:N]);
  L = ((ALFAR + ALFAI * i) ./ BETA).';
  L = sort(L);
  if isempty(S)
    G = R \ ((B)' * X);
  else
    G = R \ ((B)' * X + (S)');
  end
  varargout{1} = X;
  if nargout > 1
    varargout{2} = L;
  end
  if nargout > 2
    varargout{3} = G;
  end
end
