%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = bvpSolve(solver, odefun, bcfun, solinit, options)
  bvpValidateFunctions(odefun, bcfun);
  [x, y, parameters] = bvpNormalizeInitialSolution(solinit);
  options = bvpNormalizeOptions(options);
  maxRefines = bvpRefinementPasses(solver, options);
  stats = struct('niterations', 0, 'nmeshpoints', length(x), ...
    'residualNorm', [], 'residualRms', [], 'nrefinements', 0, ...
    'maxDefect', [], 'rmsDefect', []);
  for refine = 1:maxRefines
    [y, parameters, currentStats] = bvpNewtonSolve(odefun, bcfun, x, y, parameters, options);
    stats.niterations = stats.niterations + currentStats.niterations;
    stats.residualNorm = currentStats.residualNorm;
    stats.residualRms = currentStats.residualRms;
    if isfield(currentStats, 'nfevals')
      stats.nfevals = currentStats.nfevals;
    end
    defects = bvpMeshDefects(odefun, x, y, parameters, options);
    stats.maxDefect = max(defects);
    stats.rmsDefect = sqrt(mean(defects(:) .^ 2));
    if refine < maxRefines && bvpCanRefine(x, options, defects, bvpTolerance(options, y(:)))
      [x, y] = bvpRefineMesh(odefun, x, y, parameters, defects, options);
      stats.nrefinements = stats.nrefinements + 1;
    end
  end
  yp = bvpEvaluateSlopes(odefun, x, y, parameters, options);
  stats.nmeshpoints = length(x);
  sol = struct('solver', solver, 'x', x, 'y', y, 'yp', yp, ...
    'parameters', parameters, 'stats', stats, ...
    'extdata', struct('slopes', yp));
end
%=============================================================================
function passes = bvpRefinementPasses(solver, options)
  if strcmp(solver, 'bvp5c')
    passes = 4;
  else
    passes = 2;
  end
  nmax = bvpget(options, 'NMax', 1000);
  if nmax < 16
    passes = 1;
  end
end
%=============================================================================
function bvpValidateFunctions(odefun, bcfun)
  if ~isa(odefun, 'function_handle') || ~isa(bcfun, 'function_handle')
    error('Nelson:ode_solvers:bvpFunctionHandlesExpected', 'BVP function handles expected.');
  end
end
%=============================================================================
function [x, y, parameters] = bvpNormalizeInitialSolution(solinit)
  if ~isstruct(solinit) || ~isfield(solinit, 'x') || ~isfield(solinit, 'y')
    error('Nelson:ode_solvers:bvpInitialSolutionStructureExpected', 'BVP initial solution structure expected.');
  end
  x = solinit.x(:).';
  y = solinit.y;
  if any(diff(x) <= 0) || size(y, 2) ~= length(x)
    error('Nelson:ode_solvers:bvpInitialSolutionInconsistentMeshValues', 'BVP initial solution has inconsistent mesh and values.');
  end
  if isfield(solinit, 'parameters')
    parameters = solinit.parameters(:).';
  else
    parameters = [];
  end
end
%=============================================================================
function options = bvpNormalizeOptions(options)
  if isempty(options)
    options = bvpset();
  elseif isstruct(options)
    options = bvpMergeOptions(bvpDefaultOptions(), options);
  else
    error('Nelson:ode_solvers:bvpOptionsStructureExpected', '%s options structure expected.', 'BVP');
  end
end
%=============================================================================
function [y, parameters, stats] = bvpNewtonSolve(odefun, bcfun, x, y, parameters, options)
  u = bvpPack(y, parameters);
  tolerance = bvpTolerance(options, u);
  stats = struct('niterations', 0, 'residualNorm', Inf, 'residualRms', Inf);
  for iter = 1:20
    residual = bvpResidual(odefun, bcfun, x, u, size(y, 1), numel(parameters), options);
    stats.residualNorm = max(abs(residual));
    stats.residualRms = sqrt(mean(abs(residual(:)) .^ 2));
    stats.niterations = iter;
    if stats.residualNorm <= tolerance
      break
    end
    [kinsolAccepted, u, kinsolStats] = bvpTryKinsol(...
      odefun, bcfun, x, u, size(y, 1), numel(parameters), tolerance, residual, options);
    if kinsolAccepted
      residual = bvpResidual(odefun, bcfun, x, u, size(y, 1), numel(parameters), options);
      stats.residualNorm = max(abs(residual));
      stats.residualRms = sqrt(mean(abs(residual(:)) .^ 2));
      stats.niterations = kinsolStats.niterations;
      stats.nfevals = kinsolStats.nfevals;
      break
    end
    jacobian = bvpJacobian(odefun, bcfun, x, u, size(y, 1), numel(parameters), residual, options);
    if size(jacobian, 1) ~= size(jacobian, 2)
      error('Nelson:ode_solvers:boundaryConditionsCloseBvpSystem', 'Boundary conditions must close the BVP system.');
    end
    step = -(jacobian \ residual);
    u = bvpDampedUpdate(odefun, bcfun, x, u, step, size(y, 1), ...
      numel(parameters), stats.residualNorm, options);
  end
  if stats.residualNorm > 10 * tolerance
    error('Nelson:ode_solvers:bvpSolverFailedMeetRequestedTolerance', 'BVP solver failed to meet the requested tolerance.');
  end
  [y, parameters] = bvpUnpack(u, size(y, 1), length(x), numel(parameters));
end
%=============================================================================
function [accepted, u, stats] = bvpTryKinsol(odefun, bcfun, x, u, n, np, tolerance, residual, options)
  accepted = false;
  stats = struct();
  if length(residual) ~= length(u) || ~bvpKinsolAvailable()
    return
  end
  try
    residualFcn = @(trial) bvpResidual(odefun, bcfun, x, trial, n, np, options);
    [candidate, stats] = __bvp_kinsol_solve__(residualFcn, u, tolerance);
    candidateResidual = residualFcn(candidate);
    if max(abs(candidateResidual)) <= max(tolerance, max(abs(residual)))
      u = candidate(:);
      accepted = true;
    end
  catch
    accepted = false;
  end
end
%=============================================================================
function tf = bvpKinsolAvailable()
  tf = true;
end
%=============================================================================
function tolerance = bvpTolerance(options, u)
  absTol = bvpget(options, 'AbsTol', 1e-6);
  relTol = bvpget(options, 'RelTol', 1e-3);
  tolerance = max(absTol, relTol * max(1, max(abs(u))));
end
%=============================================================================
function u = bvpPack(y, parameters)
  u = [y(:); parameters(:)];
end
%=============================================================================
function [y, parameters] = bvpUnpack(u, n, m, np)
  y = reshape(u(1:n * m), n, m);
  parameters = u(n * m + (1:np)).';
end
%=============================================================================
function jacobian = bvpJacobian(odefun, bcfun, x, u, n, np, residual, options)
  fJacobian = bvpget(options, 'FJacobian', []);
  bcJacobian = bvpget(options, 'BCJacobian', []);
  if isempty(fJacobian) || isempty(bcJacobian)
    jacobian = bvpFiniteDifferenceJacobian(odefun, bcfun, x, u, n, np, residual, options);
  else
    jacobian = bvpAnalyticJacobian(odefun, fJacobian, bcJacobian, x, u, n, np, options);
  end
end
%=============================================================================
function jacobian = bvpFiniteDifferenceJacobian(odefun, bcfun, x, u, n, np, residual, options)
  jacobian = zeros(length(residual), length(u));
  for k = 1:length(u)
    delta = sqrt(eps) * max(1, abs(u(k)));
    trial = u;
    trial(k) = trial(k) + delta;
    trialResidual = bvpResidual(odefun, bcfun, x, trial, n, np, options);
    jacobian(:, k) = (trialResidual - residual) / delta;
  end
end
%=============================================================================
function u = bvpDampedUpdate(odefun, bcfun, x, u, step, n, np, residualNorm, options)
  factor = 1;
  for k = 1:8
    trial = u + factor * step;
    trialResidual = bvpResidual(odefun, bcfun, x, trial, n, np, options);
    if max(abs(trialResidual)) < residualNorm
      u = trial;
      return
    end
    factor = 0.5 * factor;
  end
  u = u + factor * step;
end
%=============================================================================
function values = bvpOdeCall(fcn, x, y, parameters, options)
  if isempty(parameters) || (nargin(fcn) ~= -1 && nargin(fcn) < 3)
    values = feval(fcn, x, y);
  else
    values = feval(fcn, x, y, parameters);
  end
  if isvector(values)
    values = values(:);
  end
  if nargin >= 5
    values = values + bvpSingularValue(options, x, y);
  end
end
%=============================================================================
function values = bvpBcCall(fcn, ya, yb, parameters)
  if isempty(parameters) || (nargin(fcn) ~= -1 && nargin(fcn) < 3)
    values = feval(fcn, ya, yb);
  else
    values = feval(fcn, ya, yb, parameters);
  end
  values = values(:);
end
%=============================================================================
function tf = bvpCanRefine(x, options, defects, tolerance)
  nmax = bvpget(options, 'NMax', 1000);
  tf = length(x) < nmax && any(defects > max(10 * tolerance, sqrt(eps)));
end
%=============================================================================
function [newX, newY] = bvpRefineMesh(odefun, x, y, parameters, defects, options)
  nmax = bvpget(options, 'NMax', 1000);
  threshold = max(median(defects), 0.25 * max(defects));
  selected = find(defects >= threshold);
  capacity = max(0, nmax - length(x));
  selected = selected(1:min(length(selected), capacity));
  newX = sort([x, 0.5 * (x(selected) + x(selected + 1))]);
  slopes = bvpEvaluateSlopes(odefun, x, y, parameters, options);
  newY = odeDevalInterpolate(x, y, newX, slopes);
end
%=============================================================================
function defects = bvpMeshDefects(odefun, x, y, parameters, options)
  slopes = bvpEvaluateSlopes(odefun, x, y, parameters, options);
  defects = zeros(1, length(x) - 1);
  for k = 1:(length(x) - 1)
    h = x(k + 1) - x(k);
    xm = 0.5 * (x(k) + x(k + 1));
    ym = odeDevalInterpolate(x(k:(k + 1)), y(:, k:(k + 1)), xm, slopes(:, k:(k + 1)));
    fm = bvpOdeCall(odefun, xm, ym, parameters, options);
    chord = (y(:, k + 1) - y(:, k)) / h;
    defects(k) = max(abs(chord - fm));
  end
end
%=============================================================================
function yp = bvpEvaluateSlopes(odefun, x, y, parameters, options)
  if strcmp(bvpget(options, 'Vectorized', 'off'), 'on')
    try
      yp = bvpOdeCall(odefun, x, y, parameters, options);
      if isequal(size(yp), size(y))
        return
      end
    catch
    end
  end
  yp = zeros(size(y));
  for k = 1:length(x)
    yp(:, k) = bvpOdeCall(odefun, x(k), y(:, k), parameters, options);
  end
end
%=============================================================================
function jacobian = bvpAnalyticJacobian(odefun, fJacobian, bcJacobian, x, u, n, np, options)
  [y, parameters] = bvpUnpack(u, n, length(x), np);
  intervals = length(x) - 1;
  jacobian = zeros(n * intervals + n + np, length(u));
  eyeN = eye(n);
  for k = 1:intervals
    row = (k - 1) * n + (1:n);
    h = x(k + 1) - x(k);
    [a0, b0] = bvpOdeJacobian(fJacobian, x(k), y(:, k), parameters, n, np, options);
    [a1, b1] = bvpOdeJacobian(fJacobian, x(k + 1), y(:, k + 1), parameters, n, np, options);
    f0 = bvpOdeCall(odefun, x(k), y(:, k), parameters, options);
    f1 = bvpOdeCall(odefun, x(k + 1), y(:, k + 1), parameters, options);
    midY = 0.5 * (y(:, k) + y(:, k + 1)) - h * (f1 - f0) / 8;
    [am, bm] = bvpOdeJacobian(fJacobian, 0.5 * (x(k) + x(k + 1)), midY, parameters, n, np, options);
    dmid0 = 0.5 * eyeN + h * a0 / 8;
    dmid1 = 0.5 * eyeN - h * a1 / 8;
    col0 = (k - 1) * n + (1:n);
    col1 = k * n + (1:n);
    jacobian(row, col0) = -eyeN - h * (a0 + 4 * am * dmid0) / 6;
    jacobian(row, col1) = eyeN - h * (4 * am * dmid1 + a1) / 6;
    if np > 0
      dmidp = -h * (b1 - b0) / 8;
      jacobian(row, n * length(x) + (1:np)) = -h * (b0 + 4 * (am * dmidp + bm) + b1) / 6;
    end
  end
  [dya, dyb, dpar] = bvpBoundaryJacobian(bcJacobian, y(:, 1), y(:, end), parameters, n, np);
  bcRows = n * intervals + (1:(n + np));
  jacobian(bcRows, 1:n) = dya;
  jacobian(bcRows, n * (length(x) - 1) + (1:n)) = dyb;
  if np > 0
    jacobian(bcRows, n * length(x) + (1:np)) = dpar;
  end
end
%=============================================================================
function [dfdy, dfdp] = bvpOdeJacobian(fcn, x, y, parameters, n, np, options)
  try
    if isempty(parameters) || (nargin(fcn) ~= -1 && nargin(fcn) < 3)
      [dfdy, dfdp] = feval(fcn, x, y);
    else
      [dfdy, dfdp] = feval(fcn, x, y, parameters);
    end
  catch
    if isempty(parameters) || (nargin(fcn) ~= -1 && nargin(fcn) < 3)
      dfdy = feval(fcn, x, y);
    else
      dfdy = feval(fcn, x, y, parameters);
    end
    dfdp = [];
  end
  dfdy = full(dfdy);
  if isempty(dfdp)
    dfdp = zeros(n, np);
  else
    dfdp = full(dfdp);
  end
  dfdy = dfdy + bvpSingularJacobian(options, x, n);
  if size(dfdy, 1) ~= n || size(dfdy, 2) ~= n || size(dfdp, 1) ~= n || size(dfdp, 2) ~= np
    error('Nelson:ode_solvers:bvpOdeJacobianInconsistentDimensions', 'BVP ODE Jacobian has inconsistent dimensions.');
  end
end
%=============================================================================
function [dya, dyb, dpar] = bvpBoundaryJacobian(fcn, ya, yb, parameters, n, np)
  try
    if isempty(parameters) || (nargin(fcn) ~= -1 && nargin(fcn) < 3)
      [dya, dyb, dpar] = feval(fcn, ya, yb);
    else
      [dya, dyb, dpar] = feval(fcn, ya, yb, parameters);
    end
  catch
    dpar = [];
    if isempty(parameters) || (nargin(fcn) ~= -1 && nargin(fcn) < 3)
      [dya, dyb] = feval(fcn, ya, yb);
    else
      [dya, dyb] = feval(fcn, ya, yb, parameters);
    end
  end
  dya = full(dya);
  dyb = full(dyb);
  if isempty(dpar)
    dpar = zeros(n + np, np);
  else
    dpar = full(dpar);
  end
  if size(dya, 1) ~= n + np || size(dya, 2) ~= n ...
      || size(dyb, 1) ~= n + np || size(dyb, 2) ~= n ...
      || size(dpar, 1) ~= n + np || size(dpar, 2) ~= np
    error('Nelson:ode_solvers:bvpBoundaryJacobianInconsistentDimensions', 'BVP boundary Jacobian has inconsistent dimensions.');
  end
end
%=============================================================================
function value = bvpSingularValue(options, x, y)
  value = zeros(size(y));
  singularTerm = bvpget(options, 'SingularTerm', []);
  if isempty(singularTerm)
    return
  end
  if ~isnumeric(singularTerm) || ~isreal(singularTerm) ...
      || size(singularTerm, 1) ~= size(y, 1) || size(singularTerm, 2) ~= size(y, 1)
    error('Nelson:ode_solvers:singulartermRealSquareMatrixMatchingState', 'SingularTerm must be a real square matrix matching the state size.');
  end
  if isscalar(x)
    value = bvpScalarSingularValue(singularTerm, x, y);
  else
    for k = 1:length(x)
      value(:, k) = bvpScalarSingularValue(singularTerm, x(k), y(:, k));
    end
  end
end
%=============================================================================
function value = bvpScalarSingularValue(singularTerm, x, y)
  value = zeros(size(y));
  product = singularTerm * y;
  if abs(x) <= eps
    if max(abs(product)) > sqrt(eps)
      error('Nelson:ode_solvers:singulartermRequiresConsistentLeftBoundaryValue', 'SingularTerm requires a consistent left boundary value.');
    end
    return
  end
  value = product / x;
end
%=============================================================================
function value = bvpSingularJacobian(options, x, n)
  value = zeros(n, n);
  singularTerm = bvpget(options, 'SingularTerm', []);
  if isempty(singularTerm)
    return
  end
  if abs(x) > eps
    value = singularTerm / x;
  end
end
%=============================================================================
