%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Modified Rosenbrock (2,3) pair of Shampine and Reichelt (the ode23s
% formula). Second-order accurate, L-stable, with an embedded third-order
% error estimate. The right-hand side and Jacobian are already expressed in
% standard form y' = F(t, y) (the mass matrix, if any, is pre-applied by
% odeRhs / odeRhsJacobian).
function [yhigh, ylow, evals] = odeRosenbrock23Step(problem, t, y, h)
  d = 1 / (2 + sqrt(2));
  e32 = 6 + sqrt(2);
  n = length(y);
  f0 = odeRhs(problem, t, y);
  [jac, jacEvals] = odeRhsJacobian(problem, t, y, f0);
  [tder, tderEvals] = odeRosenbrockTimeDerivative(problem, t, y, f0);
  W = eye(n, n) - h * d * jac;
  k1 = W \ (f0 + h * d * tder);
  f1 = odeRhs(problem, t + 0.5 * h, y + 0.5 * h * k1);
  k2 = W \ (f1 - k1) + k1;
  yhigh = y + h * k2;
  f2 = odeRhs(problem, t + h, yhigh);
  k3 = W \ (f2 - e32 * (k2 - f1) - 2 * (k1 - f0) + h * d * tder);
  errorEstimate = (h / 6) * (k1 - 2 * k2 + k3);
  ylow = yhigh - errorEstimate;
  evals = 3 + tderEvals + jacEvals;
end
%=============================================================================
function [tder, evals] = odeRosenbrockTimeDerivative(problem, t, y, f0)
  % Finite-difference approximation of the partial derivative df/dt, needed
  % for non-autonomous problems. It vanishes for autonomous right-hand sides.
  delta = sqrt(eps) * max(1, abs(t));
  ft = odeRhs(problem, t + delta, y);
  tder = (ft - f0) / delta;
  evals = 1;
end
%=============================================================================
