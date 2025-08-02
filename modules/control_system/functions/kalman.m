%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = kalman(varargin)
  % Design Kalman filter for state estimation
  % [kalmf, L, P, M, Z] = kalman(sys, Q, R, N)
  % [kalmf, L, P, M, Z] = kalman(sys, Q, R, N, sensors, known)
  
  narginchk(2, 6);
  nargoutchk(0, 5);
  
  [a, b, c, d, e, Ts, Q, R, N, sensors, known, md, rrn, nu] = checkInputArguments(varargin{:});
  
  [qb, rb, nb, b, d] = computedReducedMatrices(b, d, Q, R, md, known, N);
  rb = symmetrize(rb);
  validateSysEMatrix(qb, rb, nb, Ts);
  
  ra = size(a, 1);
  [P, K] = steadyStateError(a, c, qb, rb, nb, e, ra, Ts);
  [kalmf, L, M, Z] = kalmanEstimator(a, b, c, d, e, Ts, ra, rrn, nu, P, K, rb);
  
  varargout{1} = kalmf;
  if nargout > 1
    varargout{2} = L;
  end
  if nargout > 2
    varargout{3} = P;
  end
  if nargout > 3
    varargout{4} = M;
  end
  if nargout > 4
    varargout{5} = Z;
  end
end
%=============================================================================
function [Q, R, N, B, D] = computedReducedMatrices(B, D, Q, R, md, k, nn)
  S = 1:md;
  S = S(:);
  S(k, :) = [];
  G = B(:, S);
  H = D(:, S);
  B = B(:, k);
  D = D(:, k);
  Hn = H * nn;
  R = R + Hn + Hn' + H * Q * H';
  N = G * (Q * H' + nn);
  Q = G * Q * G';
end
%=============================================================================
function R = symmetrize(R)
  R = (R + R') / 2;
end
%=============================================================================
function [kEstim, L, M, Z] = kalmanEstimator(A, B, C, D, E, Ts, rA, rrn, nu, P, K, rB)
  M = [];
  L = K';
  aEstim = A - L * C;
  bEstim = [B - L * D , L];
  
  if (Ts == 0) || (Ts == -2)
    [M, Z, cEstim, dEstim] = continuousTimeEstimation(C, D, rA, rrn, nu);
  else
    [M, Z, cEstim, dEstim] = discreteTimeEstimation(C, D, rA, rrn, nu, P, rB);
  end
  
  kEstim = ss(aEstim, bEstim, cEstim, dEstim, Ts);
  kEstim.E = E;
end
%=============================================================================
function [M, Z, cEstim, dEstim] = continuousTimeEstimation(C, D, rA, rrn, nu)
  M = [];
  Z = [];
  cEstim = [C; eye(rA)];
  dEstim = [D, zeros(rrn); zeros(rA, nu + rrn)];
end
%=============================================================================
function [M, Z, cEstim, dEstim] = discreteTimeEstimation(C, D, rA, rrn, nu, P, rB)
  M = P * C' / (rB + C * P * C');
  Z = symmetrize(P - M * (C * P));
  CM = C * M;
  eyeLessCM = eye(rrn) - CM;
  cEstim = [eyeLessCM * C; eye(rA) - M * C];
  dEstim = [eyeLessCM * D, CM; -M * D, M];
end
%=============================================================================
function [P, K] = steadyStateError(A, C, Q, R, N, E, rA, Ts)
  if rA == 0
    P = [];
    K = [];
    return
  end
  % Riccati equation
  if (Ts == 0) || (Ts == -2)
    % continuous case
    [P, dummy, K] = care(A', C', Q, R, N, E');
  else
    % discrete case
    [P, dummy, K] = dare(A', C', Q, R, N, E');
  end
end
%=============================================================================
function validateSysEMatrix(Q, R, N, Ts)
  [dummy, V] = eig(R);
  realEigenvalues = real(diag(V));
  if ~isempty(realEigenvalues) && (min(realEigenvalues) < 0 || ((Ts == 0 || Ts == -2) && min(realEigenvalues) == 0))
    error(_('Wrong value for argument #1: sys.E matrix must be positive definite.'));
  end
end
%=============================================================================
function [R, N ] = validateQR(Q, R, pd, md, nbArgs)
  [rQ, cQ] = size(Q);
  [rR, cR] = size(R);
  
  if rQ ~= cQ || rQ > md
    error(_('Wrong size for argument #2: real square matrix expected.')); 
  elseif (rR ~= cR || rR > pd)
    error(('Wrong size for argument #3: real square matrix expected.'));
  elseif (rR == 0 || ((nbArgs < 5) && (rR ~= pd)))
    error('Wrong size in argument #3.');
  end
  R = rR;
  N = (md - rQ);
end
%=============================================================================
function checkIndices(indices, maxValue, expectedLength, argumentNumber)
  if (any(indices <= 0) || any(indices > maxValue) || length(indices) ~= expectedLength)
    msg = sprintf(_('Wrong value or size in arguments #%d: valid index expected and expected length %d expected.'), argumentNumber, expectedLength);
    error(msg);
  end
end
%=============================================================================
function [a, b, c, d, e, Ts, Q, R, N, sensors, known, md, rrn, nu] = checkInputArguments(varargin)
  sys = varargin{1};
  if ~isa(sys, 'ss')
    error(_('Wrong type for argument #1: state space model expected.'));
  end
  
  Q = varargin{2};
  mustBeNumeric(Q, 2);
  mustBeReal(Q, 2);
  
  R = varargin{3};
  mustBeNumeric(R, 3); 
  mustBeReal(R, 3);
  
  
  if nargin > 3
    N = varargin{4};
    if isempty(N)
      N = zeros(size(Q, 1), size(R, 1));
    end    
  else
    N = zeros(size(Q, 1), size(R, 1));
  end
  mustBeNumeric(N, 4);
  
  if nargin > 4
    sensors = varargin{5};
    mustBeNumeric(sensors, 5);
    if ~isempty(sensors)
      if ~isvector(sensors)
        error(_('Wrong size for argument #5: vector expected.'));
      end
      sensors = sensors(:);
    end
  end
  
  if nargin > 5
    known = varargin{6};
    mustBeNumeric(known, 6);
    if ~isempty(known)
      if ~isvector(known)
        error(_('Wrong size for argument #5: vector expected.'));
      end
      known = known(:);
    end
  end
  
  a = sys.A;
  b = sys.B;
  c = sys.C;
  d = sys.D;
  e = sys.E;
  
  Ts = sys.Ts;
  if ~isempty(e) && (rcond(e) <  eps())
    error(_('Wrong value in argument #1: sys.E is a singular matrix.'));
  end
  if isempty(e)
    if ~isempty(a)
      e = eye(size(a, 1), size(a, 2));
    end
  end
  
  [pd, md] = size(d);
  
  [rrn, nu] = validateQR(Q, R, pd, md, nargin);
  
  if nargin < 5    
    sensors = [1:pd];
  else
    checkIndices(sensors, pd, rrn, 5);
  end
  
  c = c(sensors, :);
  d = d(sensors, :);
  
  if nargin < 6
    known = [1:nu];
  else
    checkIndices(known, md, nu, 6);
  end
end
%=============================================================================
