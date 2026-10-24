%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [yhigh, ylow, evals] = odeLinearlyImplicitTrapezoidStep(problem, t, y, h)
  f0 = odeRhs(problem, t, y);
  [jac0, jacEvals0] = odeRhsJacobian(problem, t, y, f0);
  ypredicted = odeLinearTrapezoidPredict(y, h, f0, jac0);
  f1 = odeRhs(problem, t + h, ypredicted);
  if ischar(problem.options.JConstant) && strcmpi(problem.options.JConstant, 'on')
    jac1 = jac0;
    jacEvals1 = 0;
  else
    [jac1, jacEvals1] = odeRhsJacobian(problem, t + h, ypredicted, f1);
  end
  yhigh = odeLinearTrapezoidCorrect(y, h, f0, ypredicted, f1, jac1);
  ylow = y + h * ((eye(length(y)) - h * jac0) \ f0);
  evals = 2 + jacEvals0 + jacEvals1;
end
%=============================================================================
function ynext = odeLinearTrapezoidPredict(y, h, f, jac)
  n = length(y);
  ynext = y + h * ((eye(n, n) - 0.5 * h * jac) \ f);
end
%=============================================================================
function ynext = odeLinearTrapezoidCorrect(y, h, f0, ypredicted, f1, jac)
  n = length(y);
  rightHandSide = y + 0.5 * h * (f0 + f1 - jac * ypredicted);
  ynext = (eye(n, n) - 0.5 * h * jac) \ rightHandSide;
end
%=============================================================================
