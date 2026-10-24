%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, exitflag, iterations, lambda] = optimActiveSetQP(H, f, A, b, Aeq, beq, lb, ub, x0, opts)
  n = length(f);
  [Aall, ball, types, vars] = optimBuildConstraints(A, b, Aeq, beq, lb, ub, n);
  if isempty(x0)
    x = optimInitialPoint(Aall, ball, types, lb, ub, n);
  else
    x = double(x0(:));
  end
  active = find(types == 0).';
  inequalities = find(types ~= 0).';
  tol = opts.ConstraintTolerance;
  maxIter = opts.MaxIter;
  lambdaAll = zeros(size(ball));
  exitflag = 0;
  for iterations = 1:maxIter
    if isempty(inequalities)
      violated = zeros(0, 1);
    else
      violated = Aall * x - ball;
      [maxViolation, idx] = max(violated(inequalities));
      if maxViolation > tol
        active = unique([active, inequalities(idx)]);
        x = projectToActive(x, activeRows(Aall, active, n), ball(active));
        continue
      end
    end
    W = activeRows(Aall, active, n);
    K = [H, W'; W, zeros(size(W, 1))];
    % The bottom block is the active-constraint residual, so the Newton step
    % drives x onto the active constraints (W*(x+p) = ball) from any start.
    % When x already satisfies them the residual is zero, matching a step that
    % keeps x on the active set.
    residual = activeResidual(W, x, ball, active);
    rhs = -[H * x + f; residual];
    stepLambda = solveLinearSystem(K, rhs);
    p = stepLambda(1:n);
    lambdaW = stepLambda(n + 1:end);
    if norm(p) <= opts.StepTolerance
      lambdaAll(:) = 0;
      lambdaAll(active) = lambdaW;
      ineqActive = active(types(active) ~= 0);
      if isempty(ineqActive) || all(lambdaAll(ineqActive) >= -tol)
        exitflag = 1;
        break
      end
      [~, removeLocal] = min(lambdaAll(ineqActive));
      active(active == ineqActive(removeLocal)) = [];
      continue
    end
    alpha = 1;
    inactive = setdiff(inequalities, active);
    blocking = [];
    if ~isempty(inactive)
      denom = Aall(inactive, :) * p;
      usable = denom > tol;
      if any(usable)
        rows = inactive(usable);
        candidates = Inf(size(inactive(:)));
        candidates(usable) = (ball(rows) - Aall(rows, :) * x) ./ denom(usable);
        [candidate, pos] = min(candidates);
        if candidate < alpha, alpha = max(0, candidate); blocking = inactive(pos); end
      end
    end
    x = x + alpha * p;
    if ~isempty(blocking)
      active = unique([active, blocking]);
    end
  end
  lambda.eqlin = lambdaAll(types == 0);
  lambda.ineqlin = lambdaAll(types == 1);
  lambda.lower = zeros(n, 1);
  lambda.upper = zeros(n, 1);
  lowerRows = find(types == 2);
  upperRows = find(types == 3);
  lambda.lower(vars(lowerRows)) = lambdaAll(lowerRows);
  lambda.upper(vars(upperRows)) = lambdaAll(upperRows);
end
%=============================================================================
function [Aall, ball, types, vars] = optimBuildConstraints(A, b, Aeq, beq, lb, ub, n)
  if isempty(A), A = zeros(0, n); b = zeros(0, 1); end
  if isempty(Aeq), Aeq = zeros(0, n); beq = zeros(0, 1); end
  if size(A, 1) == 0 || size(A, 2) == 0, A = zeros(0, n); b = zeros(0, 1); end
  if size(Aeq, 1) == 0 || size(Aeq, 2) == 0, Aeq = zeros(0, n); beq = zeros(0, 1); end
  if isempty(lb), lb = -Inf(n, 1); end
  if isempty(ub), ub = Inf(n, 1); end
  lower = find(isfinite(lb(:)));
  upper = find(isfinite(ub(:)));
  lowerA = boundRows(lower, n, -1);
  upperA = boundRows(upper, n, 1);
  lowerB = -double(lb(lower)); lowerB = lowerB(:);
  upperB = double(ub(upper)); upperB = upperB(:);
  Aall = [double(Aeq); double(A); lowerA; upperA];
  ball = [double(beq(:)); double(b(:)); lowerB; upperB];
  types = [zeros(size(Aeq, 1), 1); ones(size(A, 1), 1); ...
    2 * ones(length(lower), 1); 3 * ones(length(upper), 1)];
  vars = [zeros(size(Aeq, 1) + size(A, 1), 1); lower(:); upper(:)];
end
%=============================================================================
function A = boundRows(indices, n, signValue)
  m = length(indices);
  A = zeros(m, n);
  if m > 0
    rows = (1:m)'; cols = indices(:);
    A(rows + (cols - 1) * m) = signValue;
  end
end
%=============================================================================
function x = optimInitialPoint(Aall, ball, types, lb, ub, n)
  if isempty(lb)
    lb = -Inf(n, 1);
  end
  if isempty(ub)
    ub = Inf(n, 1);
  end
  x = zeros(n, 1);
  finiteLower = isfinite(lb(:));
  finiteUpper = isfinite(ub(:));
  x(finiteLower & finiteUpper) = (lb(finiteLower & finiteUpper) + ub(finiteLower & finiteUpper)) / 2;
  x(finiteLower & ~finiteUpper) = lb(finiteLower & ~finiteUpper);
  x(~finiteLower & finiteUpper) = ub(~finiteLower & finiteUpper);
  eqRows = find(types == 0);
  if ~isempty(eqRows)
    x = projectToActive(x, activeRows(Aall, eqRows, n), ball(eqRows));
  end
end
%=============================================================================
function W = activeRows(A, rows, n)
  if isempty(rows)
    W = zeros(0, n);
  else
    W = A(rows, :);
  end
end
%=============================================================================
function residual = activeResidual(W, x, ball, active)
  if isempty(W)
    residual = zeros(0, 1);
  else
    residual = W * x - ball(active(:));
  end
end
%=============================================================================
function x = projectToActive(x, W, c)
  if isempty(W), return; end
  x = x - W' * solveLinearSystem(W * W', W * x - c);
end
%=============================================================================
function x = solveLinearSystem(A, b)
  try
    x = A \ b;
    if all(isfinite(x))
      return
    end
  catch
  end
  x = pinv(A) * b;
end
%=============================================================================
