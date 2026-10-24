%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [accepted, ynext, err, evals] = odeTryStep(problem, t, y, h, solverState)
  if strcmp(problem.solver, 'ode23')
    [yhigh, ylow, evals] = odeBogackiShampineStep(problem, t, y, h);
  elseif strcmp(problem.solver, 'ode113')
    [yhigh, ylow, evals] = odeAdamsMoultonStep(problem, t, y, h, solverState);
  elseif strcmp(problem.solver, 'ode78')
    [yhigh, ylow, evals] = odeVerner78Step(problem, t, y, h);
  elseif strcmp(problem.solver, 'ode89')
    [yhigh, ylow, evals] = odeVerner89Step(problem, t, y, h);
  elseif strcmp(problem.solver, 'ode23t')
    [yhigh, ylow, evals] = odeLinearlyImplicitTrapezoidStep(problem, t, y, h);
  elseif strcmp(problem.solver, 'ode23tb')
    [yhigh, ylow, evals] = odeTRBDF2Step(problem, t, y, h);
  elseif strcmp(problem.solver, 'ode15s')
    if odeUseFirstOrderBDF(problem)
      [yhigh, ylow, evals] = odeLinearlyImplicitEulerFirstOrderStep(problem, t, y, h);
    else
      [yhigh, ylow, evals] = odeBDFStep(problem, t, y, h, solverState);
    end
  elseif strcmp(problem.solver, 'ode23s')
    [yhigh, ylow, evals] = odeRosenbrock23Step(problem, t, y, h);
  elseif odeIsLinearlyImplicitEulerSolver(problem.solver)
    [yhigh, ylow, evals] = odeLinearlyImplicitEulerStep(problem, t, y, h);
  else
    [yhigh, ylow, evals] = odeDormandPrinceStep(problem, t, y, h);
  end
  err = odeErrorNorm(problem, y, yhigh, ylow);
  accepted = err <= 1;
  ynext = yhigh;
end

function tf = odeIsLinearlyImplicitEulerSolver(solver)
  tf = false;
end
%=============================================================================
function tf = odeUseFirstOrderBDF(problem)
  tf = ~isempty(problem.options.MaxOrder) && problem.options.MaxOrder == 1;
end
%=============================================================================
