%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% [Ar, Br, Cr, Dr] = NFlow.internal.reduceDescriptor(A, B, C, D, mask)
%
% Reduce a LINEAR descriptor (semi-explicit index-1 DAE) system to an explicit
% state-space ODE by eliminating its algebraic states (Schur complement). This
% is the core of the acausal code-generation: a linear electrical /
% thermal island linearises to
%       E xdot = A x + B u ,  y = C x + D u ,  E = diag(mask)
% where mask(i)=1 marks a DIFFERENTIAL state (real xdot) and mask(i)=0 an
% ALGEBRAIC state (node potential / branch current, xdot has no meaning). The
% algebraic rows are the constraint 0 = Aad*xd + Aaa*xa + Ba*u, so
%       xa = -Aaa \ (Aad*xd + Ba*u)
% and substituting back yields the reduced system over the differential states
% xd only, which the (multi-state, scalar-I/O) stateSpace code generator emits:
%       Ar = Add - Ada*(Aaa\Aad)
%       Br = Bd  - Ada*(Aaa\Ba)
%       Cr = Cd  - Ca *(Aaa\Aad)
%       Dr = D   - Ca *(Aaa\Ba)
%
% Inputs:
%   A     nx x nx system Jacobian (row-major full descriptor form)
%   B     nx x nu input matrix
%   C     ny x nx output matrix
%   D     ny x nu feedthrough matrix
%   mask  length-nx vector, 1 = differential state, 0 = algebraic state
%
% Outputs: the reduced (Ar, Br, Cr, Dr) over the nd = sum(mask) differential
% states. When there is no algebraic state the system is returned unchanged.
%
% Errors if the algebraic block Aaa is singular (a higher-index DAE, outside
% this curated linear tier).
%=============================================================================
function [Ar, Br, Cr, Dr] = reduceDescriptor(A, B, C, D, mask)
  narginchk(5, 5);
  nx = size(A, 1);
  if size(A, 2) ~= nx
    error('Nelson:reduceDescriptor:nFlowInternalReduceDescriptorASquare2', 'NFlow.internal.reduceDescriptor: A must be square.');
  end
  mask = logical(mask(:).');
  if numel(mask) ~= nx
    error('Nelson:reduceDescriptor:nFlowInternalReduceDescriptorMaskLength2', 'NFlow.internal.reduceDescriptor: mask length must equal size(A, 1).');
  end
  if isempty(B); B = zeros(nx, 0); end
  if isempty(C); C = zeros(0, nx); end
  nu = size(B, 2);
  ny = size(C, 1);
  if isempty(D); D = zeros(ny, nu); end

  d = find(mask); % differential state indices
  a = find(~mask); % algebraic state indices

  Add = A(d, d);
  Bd = B(d, :);
  Cd = C(:, d);

  if isempty(a)
    % Already an explicit ODE (pure differential): nothing to eliminate.
    Ar = Add; Br = Bd; Cr = Cd; Dr = D;
    return
  end

  Ada = A(d, a);
  Aad = A(a, d);
  Aaa = A(a, a);
  Ba = B(a, :);
  Ca = C(:, a);

  if rcond(Aaa) < eps
    error('Nelson:nflow_blocks:singularAlgebraicBlock', 'NFlow.internal.reduceDescriptor: the algebraic block is singular (higher-index DAE); this island is outside the linear reducible tier.');
  end

  % Solve the algebraic block once for both the state and input couplings:
  %   Kad = Aaa \ Aad ,  Kbu = Aaa \ Ba   =>   xa = -Kad*xd - Kbu*u.
  K = Aaa \ [Aad, Ba];
  Kad = K(:, 1:numel(d));
  Kbu = K(:, numel(d) + 1:end);

  Ar = Add - Ada * Kad;
  Br = Bd - Ada * Kbu;
  Cr = Cd - Ca * Kad;
  Dr = D - Ca * Kbu;
end
%=============================================================================
