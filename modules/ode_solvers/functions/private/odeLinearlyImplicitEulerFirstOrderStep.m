%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [yhigh, ylow, evals] = odeLinearlyImplicitEulerFirstOrderStep(problem, t, y, h)
  f0 = odeRhs(problem, t, y);
  [jac0, jacEvals0] = odeRhsJacobian(problem, t, y, f0);
  yfull = odeLinearlyImplicitEulerFirstOrderAdvance(y, h, f0, jac0);

  hhalf = h / 2;
  yhalf = odeLinearlyImplicitEulerFirstOrderAdvance(y, hhalf, f0, jac0);
  fhalf = odeRhs(problem, t + hhalf, yhalf);
  if ischar(problem.options.JConstant) && strcmpi(problem.options.JConstant, 'on')
    jachalf = jac0;
    jacEvalsHalf = 0;
  else
    [jachalf, jacEvalsHalf] = odeRhsJacobian(problem, t + hhalf, yhalf, fhalf);
  end
  ytwohalf = odeLinearlyImplicitEulerFirstOrderAdvance(yhalf, hhalf, fhalf, jachalf);
  yhigh = ytwohalf;
  ylow = yfull;
  evals = 2 + jacEvals0 + jacEvalsHalf;
end
%=============================================================================
function ynext = odeLinearlyImplicitEulerFirstOrderAdvance(y, h, f, jac)
  n = length(y);
  ynext = y + h * ((eye(n, n) - h * jac) \ f);
end
%=============================================================================
