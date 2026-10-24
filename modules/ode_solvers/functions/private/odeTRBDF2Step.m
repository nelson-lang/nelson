%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% TR-BDF2 step (trapezoid stage to t + gamma*h followed by a variable
% coefficient BDF2 stage to t + h, gamma = 2 - sqrt(2)) with the standard
% embedded third-order error estimate of Hosea and Shampine, filtered by the
% stage iteration matrix for stiff robustness. Both implicit stages are
% solved with a small modified-Newton iteration so that an inexact supplied
% Jacobian does not leave an uncontrolled solve residual.
function [yhigh, ylow, evals] = odeTRBDF2Step(problem, t, y, h)
  gamma = 2 - sqrt(2);
  f0 = odeRhs(problem, t, y);
  [jac0, jacEvals0] = odeRhsJacobian(problem, t, y, f0);
  htrap = gamma * h;
  ystagePredicted = odeTRBDF2TrapezoidPredict(y, htrap, f0, jac0);
  fstagePredicted = odeRhs(problem, t + htrap, ystagePredicted);
  if strcmpi(problem.options.JConstant, 'on')
    jacStage = jac0;
    jacEvalsStage = 0;
  else
    [jacStage, jacEvalsStage] = odeRhsJacobian(problem, t + htrap, ystagePredicted, fstagePredicted);
  end
  [ystage, fstage, stageEvals] = odeTRBDF2TrapezoidSolve(...
    problem, t, y, htrap, f0, ystagePredicted, fstagePredicted, jacStage);
  yendPredicted = odeTRBDF2BDFPredict(y, ystage, h, fstage, gamma);
  fendPredicted = odeRhs(problem, t + h, yendPredicted);
  if strcmpi(problem.options.JConstant, 'on')
    jacEnd = jac0;
    jacEvalsEnd = 0;
  else
    [jacEnd, jacEvalsEnd] = odeRhsJacobian(problem, t + h, yendPredicted, fendPredicted);
  end
  [yhigh, fend, endEvals] = odeTRBDF2BDFSolve(...
    problem, t, y, ystage, h, gamma, yendPredicted, fendPredicted, jacEnd);
  % Embedded third-order error estimate (difference between the TR-BDF2
  % weights and the third-order interpolatory quadrature on the stage
  % nodes), filtered through the stage iteration matrix.
  sq2 = sqrt(2);
  rawEstimate = h * (((sq2 - 1) / 3) * f0 - (1 / 3) * fstage + ((2 - sq2) / 3) * fend);
  d = 1 - sq2 / 2;
  n = length(y);
  filteredEstimate = (eye(n, n) - h * d * jacEnd) \ rawEstimate;
  ylow = yhigh - filteredEstimate;
  evals = 3 + stageEvals + endEvals + jacEvals0 + jacEvalsStage + jacEvalsEnd;
end
%=============================================================================
function [ystage, fstage, evals] = odeTRBDF2TrapezoidSolve(...
    problem, t, y, htrap, f0, ypredicted, fpredicted, jac)
  ystage = ypredicted;
  fstage = fpredicted;
  evals = 0;
  for iteration = 1:4
    ynext = odeTRBDF2TrapezoidCorrect(y, htrap, f0, ystage, fstage, jac);
    delta = ynext - ystage;
    ystage = ynext;
    scale = problem.options.AbsTol + abs(ystage) * problem.options.RelTol;
    fstage = odeRhs(problem, t + htrap, ystage);
    evals = evals + 1;
    if max(abs(delta) ./ scale) < 0.05
      break
    end
  end
end
%=============================================================================
function [yend, fend, evals] = odeTRBDF2BDFSolve(...
    problem, t, y, ystage, h, gamma, ypredicted, fpredicted, jac)
  a0 = (1 - gamma) / (gamma * h);
  a1 = -1 / (gamma * (1 - gamma) * h);
  a2 = (2 - gamma) / ((1 - gamma) * h);
  n = length(y);
  id = eye(n, n);
  yend = ypredicted;
  fend = fpredicted;
  evals = 0;
  for iteration = 1:4
    rightHandSide = fend - jac * yend - a0 * y - a1 * ystage;
    ynext = (a2 * id - jac) \ rightHandSide;
    delta = ynext - yend;
    yend = ynext;
    scale = problem.options.AbsTol + abs(yend) * problem.options.RelTol;
    fend = odeRhs(problem, t + h, yend);
    evals = evals + 1;
    if max(abs(delta) ./ scale) < 0.05
      break
    end
  end
end
%=============================================================================
function ynext = odeTRBDF2BDFPredict(y, ystage, h, fstage, gamma)
  a0 = (1 - gamma) / (gamma * h);
  a1 = -1 / (gamma * (1 - gamma) * h);
  a2 = (2 - gamma) / ((1 - gamma) * h);
  ynext = (fstage - a0 * y - a1 * ystage) / a2;
end
%=============================================================================
function ynext = odeTRBDF2TrapezoidPredict(y, h, f, jac)
  n = length(y);
  ynext = y + h * ((eye(n, n) - 0.5 * h * jac) \ f);
end
%=============================================================================
function ynext = odeTRBDF2TrapezoidCorrect(y, h, f0, ypredicted, f1, jac)
  n = length(y);
  rightHandSide = y + 0.5 * h * (f0 + f1 - jac * ypredicted);
  ynext = (eye(n, n) - 0.5 * h * jac) \ rightHandSide;
end
%=============================================================================
