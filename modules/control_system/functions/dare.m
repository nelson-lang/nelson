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
function varargout = dare(varargin)
  narginchk(3, 6);
  nargoutchk(0, 3);

  A = varargin{1};
  B = varargin{2};
  Q = varargin{3};
  mustBeNumeric(A, 1);
  mustBeNumeric(B, 2);
  mustBeNumeric(Q, 3);

  [m, n] = size(B);
  [R, S, E] = parseRiccatiOptions(varargin, m, n);

  [rA, cA] = size(A);
  if (rA ~= m) || (cA ~= m)
    error('Nelson:control_system:wrongSize', 'Wrong size for argument #%d: %s expected.', 1, 'square matrix');
  end

  if ((size(Q, 1) ~= size(Q, 2)) || (size(R, 1) ~= size(R, 2)))
    error('Nelson:control_system:wrongSize:wrongSizeArgumentsQRSquare', 'Wrong size for arguments: Q, R square matrix expected.');
  end

  if (size(A, 1) ~= m)
    error('Nelson:control_system:wrongSize:wrongSizeArgumentsNNNumberX', 'Wrong size for arguments #%d and #%d: Same number of rows expected.', 1, 2);
  end

  if (size(R, 2) ~= n)
    error('Nelson:control_system:wrongSize:wrongSizeArgumentsNNNumber', 'Wrong size for arguments #%d and #%d: Same number of columns expected.', 2, 4);
  end

  if ~isempty(S) && ((size(S, 1) ~= m) || (size(S, 2) ~= n))
    error('Nelson:control_system:wrongSize:wrongSizeArgumentsNNSize', 'Wrong size for arguments #%d and #%d: Same size expected.', 2, 5);
  end

  if ~isempty(E) && ((size(E, 1) ~= size(A, 1)) || (size(E, 2) ~= size(A, 2)))
    error('Nelson:control_system:wrongSize:wrongSizeArgumentsNNNumberX', 'Wrong size for arguments #%d and #%d: Same number of rows expected.', 1, 6);
  end

  if (size(A, 1) ~= size(Q, 1)) && (size(A, 2) ~= size(Q, 2))
    error('Nelson:control_system:wrongSize:wrongSizeArgumentsNNSize', 'Wrong size for arguments #%d and #%d: Same size expected.', 1, 3);
  end

  mustBeFinite(A, 1);
  mustBeFinite(B, 2);
  mustBeFinite(Q, 3);
  mustBeFinite(R, 4);

  X = [];
  L = [];
  G = [];

  DICO = 'D';
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
    SCAL = 'N';
    ACC = 'N';
    [RCOND, X, ALFAR, ALFAI, BETA, S_OUT, T, U, IWARN, INFO] = slicot_sg02ad(DICO, JOBB, FACT, UPLO, JOBL, SCAL, SORT, ACC, P, A, E, B, Q, R, SorB, TOL);
    if (INFO(1) ~= 0)
      error('Nelson:control_system:riccatiSolveFailure', 'Unable to solve the specified Riccati equation.');
    end
    if (IWARN(1) == 1)
      warning('Nelson:control_system:accuracyCompromised', 'The accuracy of the solution could be compromised by inadequate scaling or eigenvalues that are in close proximity to the stability domain''s boundary.');
    end
  else
    SCAL = 'N';
    ACC = 'N';
    [RCOND, X, ALFAR, ALFAI, BETA, S_OUT, T, U, INFO] = slicot_sb02od(DICO, JOBB, FACT, UPLO, JOBL, SORT, P, A, B, Q, R, SorB, TOL);
    if (INFO(1) ~= 0)
      error('Nelson:control_system:riccatiSolveFailure', 'Unable to solve the specified Riccati equation.');
    end
  end
  N = size(A, 1);
  ALFAR = ALFAR([1:N]);
  ALFAI = ALFAI([1:N]);
  BETA = BETA([1:N]);

  L = ((ALFAR + ALFAI * i) ./ BETA).';
  if isempty(S)
    G = (R + B' * X * B) \ (B' * X * A);
  else
    G = (R + B' * X * B) \ (B' * X * A + S');
  end
  varargout{1} = X;
  if nargout > 1
    varargout{2} = L;
  end
  if nargout > 2
    varargout{3} = G;
  end
end
%=============================================================================
function [R, S, E] = parseRiccatiOptions(args, stateCount, inputCount)
  if length(args) >= 4
    R = args{4};
    mustBeNumeric(R, 4);
    if ~issymmetric(R, 0)
      error('Nelson:control_system:wrongValue', 'Wrong value for argument #%d: %s expected.', 4, 'symmetric matrix');
    end
  else
    R = eye(inputCount, inputCount);
  end
  if length(args) >= 5
    S = args{5};
  else
    S = zeros(stateCount, inputCount);
  end
  if length(args) >= 6
    E = args{6};
  else
    E = eye(stateCount, stateCount);
  end
  mustBeNumeric(S, 5);
  mustBeNumeric(E, 6);
end
%=============================================================================
