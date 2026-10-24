%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = solve(prob, x0, varargin)
  if nargin < 1
    error('Nelson:optimization:optimizationProblemExpected', 'Optimization problem expected.');
  end
  if nargin < 2, x0 = struct(); end
  if ~isa(prob, 'optim.problemdef.OptimizationProblem')
    error('Nelson:optimization:optimizationProblemExpected', 'Optimization problem expected.');
  end
  problem = prob2struct(prob, x0);
  if isempty(varargin)
    options = [];
  else
    options = varargin{1};
  end
  problem.options = options;
  switch problem.solver
    case 'linprog'
      [x, fval, exitflag, output] = linprog(problem);
    case 'intlinprog'
      [x, fval, exitflag, output] = intlinprog(problem);
    case 'quadprog'
      [x, fval, exitflag, output] = quadprog(problem);
    case 'fmincon'
      [x, fval, exitflag, output] = fmincon(problem);
    otherwise
      [x, fval, exitflag, output] = fminsearch(problem.objective, problem.x0, options);
  end
  addOffset = isfield(problem, 'objectiveOffset') ...
    && (strcmp(problem.solver, 'linprog') || strcmp(problem.solver, 'intlinprog') ...
    || strcmp(problem.solver, 'quadprog'));
  if strcmpi(prob.ObjectiveSense, 'maximize') && ~isempty(fval)
    if addOffset
      fval = fval + problem.objectiveOffset;
    end
    fval = -fval;
  elseif addOffset && ~isempty(fval)
    fval = fval + problem.objectiveOffset;
  end
  if isempty(x)
    sol = emptySolution(prob);
  else
    sol = unpackValues(prob, x);
  end
  outputs = {sol, fval, exitflag, output};
  varargout = outputs(1:nargout);
end
%=============================================================================
function sol = emptySolution(prob)
  sizes = variableSizes(prob);
  names = variableNames(prob, sizes);
  sol = struct();
  for k = 1:length(names)
    sol.(names{k}) = [];
  end
end
%=============================================================================
function names = variableNames(prob, sizes)
  names = objectiveVariables(prob);
  extraNames = fieldnames(sizes);
  names = unique([names, extraNames(:)'], 'stable');
end
%=============================================================================
