%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [fun, x0, A, b, Aeq, beq, lb, ub, nonlcon, options] = optimFminconProblem(problem)
  if ~isfield(problem, 'objective') || ~isfield(problem, 'x0')
    error('Nelson:optimization:problemMissingObjective', 'Problem structure must contain %s fields.', 'objective and x0');
  end
  if ~isfield(problem, 'solver') || ~isfield(problem, 'options')
    error('Nelson:optimization:invalidProblemStructure', 'Problem structure must contain %s fields.', 'solver and options');
  end
  if ~strcmpi(char(problem.solver), 'fmincon')
    error('Nelson:optimization:invalidProblemSolver', 'Problem solver must be %s.', 'fmincon');
  end
  fun = problem.objective;
  x0 = problem.x0;
  A = getField(problem, 'Aineq', getField(problem, 'A', []));
  b = getField(problem, 'bineq', getField(problem, 'b', []));
  Aeq = getField(problem, 'Aeq', []);
  beq = getField(problem, 'beq', []);
  lb = getField(problem, 'lb', []);
  ub = getField(problem, 'ub', []);
  nonlcon = getField(problem, 'nonlcon', []);
  options = getField(problem, 'options', []);
end
%=============================================================================
function value = getField(st, name, defaultValue)
  if isfield(st, name)
    value = st.(name);
  else
    value = defaultValue;
  end
end
%=============================================================================
