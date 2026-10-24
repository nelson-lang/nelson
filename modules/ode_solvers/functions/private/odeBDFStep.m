%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Variable-order, variable-step BDF kernel for ode15s (orders 2 to 5). The
% order grows with the accumulated solution history: BDF-k differentiates
% the polynomial interpolating the new point and the last k accepted points,
% and the local error is estimated with a Milne device against the
% extrapolant of the same order through the history alone. The implicit
% equation is solved with a modified-Newton iteration so an inexact
% supplied Jacobian cannot leave an uncontrolled solve residual. With fewer
% than three history points the BDF2 kernel (with its own startup) is used.
function [yhigh, ylow, evals] = odeBDFStep(problem, t, y, h, solverState)
  k = odeBDFOrderFromState(problem, solverState);
  if k <= 2
    [yhigh, ylow, evals] = odeBDF2Step(problem, t, y, h, solverState);
    return
  end
  f0 = odeRhs(problem, t, y);
  [jac, jacEvals] = odeRhsJacobian(problem, t, y, f0);
  count = length(solverState.t);
  tnew = t + h;
  historyT = solverState.t((count - k + 1):count);
  historyY = solverState.y(:, (count - k + 1):count);
  weights = odeBDFDerivativeWeights([historyT, tnew], tnew);
  % The derivative weights sum to zero analytically; enforcing it exactly
  % makes the formula reproduce constant solution components to rounding.
  weights(k + 1) = -sum(weights(1:k));
  historyTerm = historyY * weights(1:k).';
  leading = weights(k + 1);
  % Predictor: extrapolation of the degree-k polynomial through the last
  % k + 1 accepted points.
  ypred = odeBDFExtrapolate(solverState, k, tnew);
  % Numerical differentiation formula modification (Shampine-Reichelt):
  % the kappa term damps the leading truncation error of BDF-k, allowing
  % larger steps at the same accuracy. BDF='on' selects the plain BDF
  % formulas (kappa = 0).
  kappa = odeNDFKappa(problem, k);
  ndfShift = kappa * sum(1 ./ (1:k)) / h;
  ynext = ypred;
  fnext = odeRhs(problem, tnew, ynext);
  newtonEvals = 1;
  n = length(y);
  iterationMatrix = (leading - ndfShift) * eye(n, n) - jac;
  for iteration = 1:4
    residual = leading * ynext + historyTerm - fnext - ndfShift * (ynext - ypred);
    delta = iterationMatrix \ (-residual);
    ynext = ynext + delta;
    scale = problem.options.AbsTol + abs(ynext) * problem.options.RelTol;
    if max(abs(delta) ./ scale) < 0.05
      break
    end
    fnext = odeRhs(problem, tnew, ynext);
    newtonEvals = newtonEvals + 1;
  end
  yhigh = ynext;
  ylow = yhigh - 1.2 * (ynext - ypred) / (k + 1);
  evals = 1 + newtonEvals + jacEvals;
end
%=============================================================================
function kappa = odeNDFKappa(problem, k)
  if ischar(problem.options.BDF) && strcmpi(problem.options.BDF, 'on')
    kappa = 0;
    return
  end
  kappas = [-0.1850, -1 / 9, -0.0823, -0.0415, 0];
  kappa = kappas(k);
end
%=============================================================================
function weights = odeBDFDerivativeWeights(nodes, x)
  % Derivative of the Lagrange basis polynomials at x.
  count = length(nodes);
  weights = zeros(1, count);
  for i = 1:count
    total = 0;
    for m = 1:count
      if m == i
        continue
      end
      product = 1 / (nodes(i) - nodes(m));
      for j = 1:count
        if j ~= i && j ~= m
          product = product * (x - nodes(j)) / (nodes(i) - nodes(j));
        end
      end
      total = total + product;
    end
    weights(i) = total;
  end
end
%=============================================================================
function ypred = odeBDFExtrapolate(solverState, k, tnew)
  count = length(solverState.t);
  nodes = solverState.t((count - k):count);
  values = solverState.y(:, (count - k):count);
  weights = zeros(1, length(nodes));
  for i = 1:length(nodes)
    weight = 1;
    for j = 1:length(nodes)
      if j ~= i
        weight = weight * (tnew - nodes(j)) / (nodes(i) - nodes(j));
      end
    end
    weights(i) = weight;
  end
  % Lagrange weights sum to one analytically; enforcing it exactly keeps
  % constant solution components exact to rounding.
  weights(end) = weights(end) + (1 - sum(weights));
  ypred = values * weights.';
end
%=============================================================================
