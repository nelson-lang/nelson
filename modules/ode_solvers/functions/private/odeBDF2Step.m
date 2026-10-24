%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [yhigh, ylow, evals] = odeBDF2Step(problem, t, y, h, solverState)
  if ~odeHasBDF2History(solverState, h)
    [yhigh, ylow, evals] = odeBDF2StartupStep(problem, t, y, h);
    return
  end

  f0 = odeRhs(problem, t, y);
  [jac0, jacEvals] = odeRhsJacobian(problem, t, y, f0);
  yprevious = solverState.y(:, size(solverState.y, 2) - 1);
  lastIndex = length(solverState.t);
  hprevious = solverState.t(lastIndex) - solverState.t(lastIndex - 1);
  coeffs = odeBDF2Coefficients(h, hprevious);
  ypredicted = odeBDF2Advance(y, yprevious, h, f0, jac0, coeffs);
  f1 = odeRhs(problem, t + h, ypredicted);
  [jac1, jacEvals1] = odeBDF2JacobianAtEnd(problem, t + h, ypredicted, f1, jac0);
  % Modified-Newton iteration on the BDF2 equation. Iterating to a small
  % residual (instead of a single correction) keeps the solve error under
  % control even when the supplied Jacobian is inexact, so the truncation
  % error estimate below remains the dominant error term.
  ybdf2 = ypredicted;
  fEnd = f1;
  newtonEvals = 0;
  for iteration = 1:4
    ynext = odeBDF2ImplicitCorrect(y, yprevious, h, ybdf2, fEnd, jac1, coeffs);
    delta = ynext - ybdf2;
    ybdf2 = ynext;
    scale = problem.options.AbsTol + abs(ybdf2) * problem.options.RelTol;
    if max(abs(delta) ./ scale) < 0.05
      break
    end
    fEnd = odeRhs(problem, t + h, ybdf2);
    newtonEvals = newtonEvals + 1;
  end
  yhigh = ybdf2;
  % Milne-device local error estimate: the corrected solution is compared
  % with the quadratic extrapolant through the solution history, giving an
  % O(h^3) estimate matching the order of the BDF2 formula (instead of the
  % O(h) implicit-Euler comparison that forced tiny steps at tight
  % tolerances).
  ypoly = odeBDF2ExtrapolatedPredictor(solverState, t, y, h);
  ylow = yhigh - 1.2 * (ybdf2 - ypoly);
  evals = 2 + newtonEvals + jacEvals + jacEvals1;
end
%=============================================================================
function ypoly = odeBDF2ExtrapolatedPredictor(solverState, t, y, h)
  count = length(solverState.t);
  tnodes = [solverState.t(count - 1), t];
  ynodes = [solverState.y(:, count - 1), y];
  if count >= 3
    tnodes = [solverState.t(count - 2), tnodes];
    ynodes = [solverState.y(:, count - 2), ynodes];
  end
  tnew = t + h;
  ypoly = zeros(size(y));
  for k = 1:length(tnodes)
    weight = 1;
    for j = 1:length(tnodes)
      if j ~= k
        weight = weight * (tnew - tnodes(j)) / (tnodes(k) - tnodes(j));
      end
    end
    ypoly = ypoly + weight * ynodes(:, k);
  end
end
%=============================================================================
function [jac, evals] = odeBDF2JacobianAtEnd(problem, t, y, f, jac0)
  if ischar(problem.options.JConstant) && strcmpi(problem.options.JConstant, 'on')
    jac = jac0;
    evals = 0;
  else
    [jac, evals] = odeRhsJacobian(problem, t, y, f);
  end
end
%=============================================================================
function [yhigh, ylow, evals] = odeBDF2StartupStep(problem, t, y, h)
  f0 = odeRhs(problem, t, y);
  [jac0, jacEvals0] = odeRhsJacobian(problem, t, y, f0);
  yfull = odeBDF2CorrectedEulerAdvance(problem, t, y, h, f0, jac0);

  hhalf = h / 2;
  yhalf = odeBDF2CorrectedEulerAdvance(problem, t, y, hhalf, f0, jac0);
  fhalf = odeRhs(problem, t + hhalf, yhalf);
  if ischar(problem.options.JConstant) && strcmpi(problem.options.JConstant, 'on')
    jachalf = jac0;
    jacEvalsHalf = 0;
  else
    [jachalf, jacEvalsHalf] = odeRhsJacobian(problem, t + hhalf, yhalf, fhalf);
  end
  ytwohalf = odeBDF2CorrectedEulerAdvance(problem, t + hhalf, yhalf, hhalf, fhalf, jachalf);

  yhigh = 2 * ytwohalf - yfull;
  ylow = ytwohalf;
  evals = 5 + jacEvals0 + jacEvalsHalf;
end
%=============================================================================
function tf = odeHasBDF2History(solverState, h)
  tf = false;
  if ~isfield(solverState, 'historyValid') || ~solverState.historyValid
    return
  end
  if ~isfield(solverState, 't') || length(solverState.t) < 2
    return
  end
  if ~isfield(solverState, 'y') || size(solverState.y, 2) < 2
    return
  end
  lastIndex = length(solverState.t);
  hprevious = solverState.t(lastIndex) - solverState.t(lastIndex - 1);
  tf = sign(hprevious) == sign(h) && abs(hprevious) > eps;
end
%=============================================================================
function coeffs = odeBDF2Coefficients(h, hprevious)
  ratio = h / hprevious;
  coeffs = [(1 + 2 * ratio) / (1 + ratio), ...
    -(1 + ratio), ratio ^ 2 / (1 + ratio)];
end
%=============================================================================
function ynext = odeBDF2CorrectedEulerAdvance(problem, t, y, h, f, jac)
  ypredicted = odeBDF2LinearlyImplicitEulerAdvance(y, h, f, jac);
  f1 = odeRhs(problem, t + h, ypredicted);
  ynext = y + h * f1;
end
%=============================================================================
function ynext = odeBDF2Advance(y, yprevious, h, f, jac, coeffs)
  n = length(y);
  identity = eye(n, n);
  alpha = coeffs(1) / h;
  rhs = f - jac * y - (coeffs(2) * y + coeffs(3) * yprevious) / h;
  ynext = (alpha * identity - jac) \ rhs;
end
%=============================================================================
function ynext = odeBDF2ImplicitCorrect(y, yprevious, h, ypredicted, f, jac, coeffs)
  n = length(y);
  identity = eye(n, n);
  alpha = coeffs(1) / h;
  rhs = f - jac * ypredicted - (coeffs(2) * y + coeffs(3) * yprevious) / h;
  ynext = (alpha * identity - jac) \ rhs;
end
%=============================================================================
function ynext = odeBDF2LinearlyImplicitEulerAdvance(y, h, f, jac)
  n = length(y);
  ynext = y + h * ((eye(n, n) - h * jac) \ f);
end
%=============================================================================
