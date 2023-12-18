%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = lqed(varargin)
  narginchk(6, 6);
  nargoutchk(0, 4);

  A = varargin{1};
  G = varargin{2};
  C = varargin{3};
  Q = varargin{4};
  R = varargin{5};
  Ts = varargin{6};

  msg = abcdchk(A, [], C, []);
  if ~isempty(msg)
    error(msg.id, msg.message);
  end

  validateInput(A, G, C, Q, R);

  [Qd, Rd] = computeDiscreteCostMatrices(A, G, Q, R, Ts);
  [p, e, k] = dare(expm(A * Ts)', C', Qd, Rd);
  
  l = p * C' / (Rd + C * p * C');
  z = (eye(size(A, 1)) - l * C) * p;
  z = symmetrize(z);

  varargout{1} = l;
  if nargout > 1
    varargout{2} = p;
  end
  if nargout > 2
    varargout{3} = z;
  end
  if nargout > 3
    varargout{4} = e;
  end
end
%=============================================================================
function validateInput(A, G, C, Q, R)
  rA = size(A, 1);
  [rG, cG] = size(G);
  rC = size(C, 1);

  if rA ~= rG
     error('A and G matrices should possess an identical number of rows.');
  elseif any(size(Q) ~= cG)
     error('Q should be a square matrix with the same number of columns as G.');
  elseif any(size(R) ~= rC)
     error('The matrix R must be square and have the same number of rows as C.');
  end
end
%=============================================================================
function [Qd, Rd] = computeDiscreteCostMatrices(A, G, Q, R, Ts)
  rA = size(A, 1);
  Za = zeros(rA); 
  M = [ -A,  G * Q * G'; Za, A'];
  phi = expm(M * Ts);
  phiA = phi(1:rA, rA + 1: 2 * rA);
  phiB = phi(rA + 1: 2 * rA, rA + 1: 2 * rA);
  Qd = phiB' * phiA;
  Qd = symmetrize(Qd);
  Rd = R/Ts;
end
%=============================================================================
function result = symmetrize(m)
  result = (m + m') / 2;
end
%=============================================================================
