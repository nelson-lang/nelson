%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, fval, exitflag, output, lambda] = quadprog(H, f, A, b, Aeq, beq, lb, ub, x0, options)
  if nargin == 1 && isstruct(H)
    problem = H;
    [H, f, A, b, Aeq, beq, lb, ub, x0, options] = unpackProblem(problem);
  else
    if nargin < 2, error('Nelson:notEnoughInputs', 'Not enough input arguments.'); end
    if nargin < 3, A = []; end
    if nargin < 4, b = []; end
    if nargin < 5, Aeq = []; end
    if nargin < 6, beq = []; end
    if nargin < 7, lb = []; end
    if nargin < 8, ub = []; end
    if nargin < 9, x0 = []; end
    if nargin < 10, options = []; end
  end
  opts = optimMergeOptions('quadprog', options);
  H = double(H); f = double(f(:));
  [A, b] = normalizeConstraintMatrix(A, b);
  [Aeq, beq] = normalizeConstraintMatrix(Aeq, beq);
  [x, exitflag, iterations, lambda] = optimActiveSetQP(H, f, A, b, Aeq, beq, lb, ub, x0, opts);
  fval = 0.5 * x' * H * x + f' * x;
  output.iterations = iterations;
  output.algorithm = 'active-set quadratic programming';
  output.constrviolation = constraintViolation(A, b, Aeq, beq, lb, ub, x);
  output.firstorderopt = norm(H * x + f, Inf);
  output.message = exitMessage(exitflag);
end
%=============================================================================
function [A, b] = normalizeConstraintMatrix(A, b)
  if isempty(A) || size(A, 1) == 0
    A = [];
    b = [];
  end
end
%=============================================================================
function [H, f, A, b, Aeq, beq, lb, ub, x0, options] = unpackProblem(problem)
  H = problem.H; f = problem.f;
  A = getFieldOrEmpty(problem, 'Aineq'); b = getFieldOrEmpty(problem, 'bineq');
  if isempty(A), A = getFieldOrEmpty(problem, 'A'); end
  if isempty(b), b = getFieldOrEmpty(problem, 'b'); end
  Aeq = getFieldOrEmpty(problem, 'Aeq'); beq = getFieldOrEmpty(problem, 'beq');
  lb = getFieldOrEmpty(problem, 'lb'); ub = getFieldOrEmpty(problem, 'ub');
  x0 = getFieldOrEmpty(problem, 'x0'); options = getFieldOrEmpty(problem, 'options');
end
%=============================================================================
function value = getFieldOrEmpty(st, name)
  if isfield(st, name), value = st.(name); else, value = []; end
end
%=============================================================================
function v = constraintViolation(A, b, Aeq, beq, lb, ub, x)
  vals = 0;
  if ~isempty(A), vals = max(vals, max([0; A * x - b(:)])); end
  if ~isempty(Aeq), vals = max(vals, max(abs(Aeq * x - beq(:)))); end
  if ~isempty(lb), vals = max(vals, max([0; lb(:) - x])); end
  if ~isempty(ub), vals = max(vals, max([0; x - ub(:)])); end
  v = vals;
end
%=============================================================================
function msg = exitMessage(exitflag)
  if exitflag > 0
    msg = _('Optimization converged.');
  else
    msg = _('Maximum number of iterations reached.');
  end
end
%=============================================================================
