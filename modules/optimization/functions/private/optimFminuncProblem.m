%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [fun, x0, options] = optimFminuncProblem(problem)
  if ~isfield(problem, 'objective') || ~isfield(problem, 'x0')
    error('Nelson:optimization:problemMissingObjective', 'Problem structure must contain %s fields.', 'objective and x0');
  end
  if ~isfield(problem, 'solver') || ~isfield(problem, 'options')
    error('Nelson:optimization:invalidProblemStructure', 'Problem structure must contain %s fields.', 'solver and options');
  end
  if ~strcmpi(char(problem.solver), 'fminunc')
    error('Nelson:optimization:invalidProblemSolver', 'Problem solver must be %s.', 'fminunc');
  end
  fun = problem.objective;
  x0 = problem.x0;
  options = problem.options;
end
%=============================================================================
