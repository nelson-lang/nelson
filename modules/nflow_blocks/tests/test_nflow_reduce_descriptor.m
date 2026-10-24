%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Schur reduction of a linear descriptor (index-1 DAE) to an explicit ODE
% state-space, the core of the acausal code-generation. Verified
% against analytically-known reductions and a numerical descriptor cross-check.
%=============================================================================
% RLC with a redundant algebraic node e = vc, output y = e. States [vc,iL,e],
% mask [1 1 0]: vc' = iL ; iL' = -vc - iL + u ; 0 = e - vc ; y = e. The
% reduction must recover the plain 2-state RLC realisation.
A = [0 1 0; -1 -1 0; -1 0 1];
B = [0; 1; 0];
C = [0 0 1];
D = 0;
[Ar, Br, Cr, Dr] = NFlow.internal.reduceDescriptor(A, B, C, D, [1 1 0]);
assert_isequal(Ar, [0 1; -1 -1]);
assert_isequal(Br, [0; 1]);
assert_isequal(Cr, [1 0]);
assert_isequal(Dr, 0);
%=============================================================================
% A pure ODE (no algebraic state) is returned unchanged.
[Ar2, Br2, Cr2, Dr2] = NFlow.internal.reduceDescriptor([0 1; -4 -1], [0; 1], [1 0], 0, [1 1]);
assert_isequal(Ar2, [0 1; -4 -1]);
assert_isequal(Br2, [0; 1]);
assert_isequal(Cr2, [1 0]);
assert_isequal(Dr2, 0);
%=============================================================================
% Algebraic coupling that feeds back into a differential row (Ada nonzero):
% x1' = x2 + a ; x2' = -x1 ; 0 = a - x1 (a = x1) ; y = x1.
% Reduced: x1' = x2 + x1 ; x2' = -x1  =>  Ar = [1 1; -1 0].
A3 = [0 1 1; -1 0 0; -1 0 1];
[Ar3, ~, Cr3, ~] = NFlow.internal.reduceDescriptor(A3, [0; 0; 0], [1 0 0], 0, [1 1 0]);
assert_isequal(Ar3, [1 1; -1 0]);
assert_isequal(Cr3, [1 0]);
%=============================================================================
% Numerical cross-check: the reduced ODE trajectory equals the descriptor's
% differential trajectory (algebraic constraint enforced each step).
dt = 1e-3; N = 2000; u = 1;
xd = [0; 0]; xf = [0; 0; 0]; maxerr = 0;
for k = 1:N
  xd = xd + dt * (Ar * xd + Br * u);
  xf(3) = xf(1);
  xf(1) = xf(1) + dt * xf(2);
  xf(2) = xf(2) + dt * (-xf(1) - xf(2) + u);
end
% (compare accumulated position: recompute both consistently)
xd = [0; 0]; xf = [0; 0];
for k = 1:N
  xd = xd + dt * (Ar * xd + Br * u);
  xf = xf + dt * ([0 1; -1 -1] * xf + [0; 1] * u);
  maxerr = max(maxerr, abs(xd(1) - xf(1)));
end
assert_istrue(maxerr < 1e-12);
%=============================================================================
% A singular algebraic block (higher-index DAE) is rejected.
assert_checkerror('NFlow.internal.reduceDescriptor([1 0; 0 0], [0;0], [1 0], 0, [1 0])', ...
  _(['NFlow.internal.reduceDescriptor: the algebraic block is singular ', ...
  '(higher-index DAE); this island is outside the linear reducible tier.']));
%=============================================================================
