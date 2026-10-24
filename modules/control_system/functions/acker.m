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
function varargout = acker(varargin)
  % K = acker(A, B, P)
  % Source from Modern Control Engineering, Ogata, 3th edition.
  narginchk(3, 3)
  nargoutchk(0, 1)

  A = varargin{1};
  B = varargin{2};
  P = varargin{3};

  [mA, nA] = size(A);
  [mB, nB] = size(B);
  if (mA ~= nA)
    error('Nelson:expectedSquare', '%s must be square.', 'Matrix A');
  end

  if (mB ~= mA)
    error('Nelson:control_system:AAndBNumRowsMismatch', 'The number of rows in matrices %s and %s must be equal.', 'A', 'B');
  end
  if nB ~= 1
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end

  % Vectorize P
  P = P(:);
  if (size(A, 1) ~= length(P))
    error('Nelson:control_system:dimagree:polesNeedDimensionMatrixA', 'Poles need to have the same dimension as matrix A')
  end

  % Create the control law gain matrix L
  %Formula from Ogata Modern Control Engineering
  Cm = ctrb(A, B); % Controllability matrix
  RealPoly = real(poly(P)); % Real polynomial of P
  L = Cm \ polyvalm(RealPoly, A);
  L = L(nA, :);

  % Check if the user has put in very bad pole locations
  P = sort(P);
  nonZeroPoles = find(P ~= 0);
  P = P(nonZeroPoles);
  % Sort the eigen values
  eigenvalues = sort(eig(A - B * L));
  % Ge the egenvalues which has non zero poles
  eigenvalues = eigenvalues(nonZeroPoles);
  % Get the absolute maximum value of P
  M = abs(P);

  % Get the difference between pole locations and eigen values
  Diff = abs(P - eigenvalues);
  if (max(Diff ./ M) > .1)
    warning('Nelson:acker:poleLocationError', 'Pole locations are in more that 10% error');
  end
  varargout{1} = L;
end
