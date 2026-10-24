%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = odeRunSensitivity(solver, odefun, tspan, y0, yp0, options, sensitivity)
  odeValidateSensitivityProblem(solver, options, sensitivity);
  if strcmp(sensitivity.Method, 'adjoint')
    sol = odeRunAdjointSensitivity(solver, odefun, tspan, y0, yp0, options, sensitivity);
    return
  end
  parameters = options.Parameters(:);
  indices = odeSensitivityParameterIndices(sensitivity, length(parameters));
  y0 = y0(:);
  if odeIsSundialsSolver(solver) && isempty(options.Events)
    sol = odeRunSundialsSensitivity(...
      solver, odefun, tspan, y0, yp0, options, sensitivity, parameters, indices);
    return
  end
  if odeSensitivityIsImplicitSolver(solver)
    sol = odeRunImplicitSensitivity(solver, odefun, tspan, y0, yp0, options, ...
      sensitivity, parameters, indices);
    return
  end
  s0 = odeSensitivityInitialValue(sensitivity, length(y0), length(indices));
  z0 = [y0; s0(:)];
  augOptions = odeSensitivityOptions(options, length(y0));
  augfun = @(t, z) odeSensitivityRhs(odefun, t, z, length(y0), parameters, indices, ...
    sensitivity, options);
  sol = odeRunSolver(solver, augfun, tspan, z0, [], augOptions);
  sol = odeSensitivitySplitSolution(sol, length(y0), length(indices));
end
%=============================================================================
function sol = odeRunSundialsSensitivity(solver, odefun, tspan, y0, yp0, options, ...
    sensitivity, parameters, indices)
  n = length(y0);
  p = length(indices);
  s0 = odeSensitivityInitialValue(sensitivity, n, p);
  sensitivityData = struct();
  sensitivityData.Parameters = parameters(:);
  sensitivityData.ParameterIndices = indices(:) - 1;
  sensitivityData.ParameterScales = max(abs(parameters(indices(:))), 1);
  sensitivityData.InitialValue = s0;
  if strcmp(solver, 'idas')
    if isempty(yp0)
      yp0 = zeros(size(y0));
    else
      yp0 = yp0(:);
    end
    sensitivityData.InitialSlope = odeSensitivityInitialSlopeImplicit(...
      odefun, tspan(1), y0, yp0, s0, sensitivity, parameters, indices, options);
  else
    sensitivityData.InitialSlope = [];
  end
  nativeOptions = options;
  nativeOptions.Sensitivity = sensitivityData;
  problem = odePrepareProblem(solver, odefun, tspan, y0, yp0, nativeOptions);
  sol = __ode_sundials_run__(problem);
  odeDisplayStats(problem, sol.stats);
end
%=============================================================================
function odeValidateSensitivityProblem(solver, options, sensitivity)
  if ~isnumeric(options.Parameters) || ~isreal(options.Parameters) || ~isvector(options.Parameters)
    error('Nelson:ode_solvers:parametersNumericVectorPerformSensitivityAnalysis', 'Parameters must be a numeric vector to perform a sensitivity analysis.');
  end
  if ~odeSensitivityIsImplicitSolver(solver) && ~isempty(sensitivity.InitialSlope)
    error('Nelson:ode_solvers:sensitivityInitialSlopesNotSupportedExplicit', 'Sensitivity initial slopes are not supported for explicit problems.');
  end
  if isfield(options, 'EventDefinitionObject') ...
      && odeSensitivityHasCallbackEvent(options.EventDefinitionObject)
    error('Nelson:ode_solvers:sensitivityEquationsEventCallbacksNotSupported', 'Sensitivity equations with event callbacks are not supported yet.');
  end
end
%=============================================================================
function tf = odeSensitivityIsImplicitSolver(solver)
  tf = strcmp(solver, 'ode15i') || strcmp(solver, 'idas');
end
%=============================================================================
function sol = odeRunImplicitSensitivity(solver, odefun, tspan, y0, yp0, options, ...
    sensitivity, parameters, indices)
  n = length(y0);
  p = length(indices);
  if isempty(yp0)
    yp0 = zeros(size(y0));
  else
    yp0 = yp0(:);
  end
  s0 = odeSensitivityInitialValue(sensitivity, n, p);
  sp0 = odeSensitivityInitialSlopeImplicit(...
    odefun, tspan(1), y0, yp0, s0, sensitivity, parameters, indices, options);
  z0 = [y0; s0(:)];
  zp0 = [yp0; sp0(:)];
  augOptions = odeSensitivityOptions(options, n);
  augOptions.InitialSlope = zp0;
  augfun = @(t, z, zp) odeSensitivityImplicitResidual(...
    odefun, t, z, zp, n, p, parameters, indices, options);
  sol = odeRunSolver(solver, augfun, tspan, z0, zp0, augOptions);
  sol = odeSensitivitySplitSolution(sol, n, p);
end
%=============================================================================
function sp0 = odeSensitivityInitialSlopeImplicit(...
    odefun, t, y, yp, s0, sensitivity, parameters, indices, options)
  n = length(y);
  p = length(indices);
  if ~isempty(sensitivity.InitialSlope)
    sp0 = odeSensitivityInitialSlopeValue(sensitivity.InitialSlope, n, p);
    return
  end
  residual = odeSensitivityImplicitPhysicalResidual(odefun, t, y, yp, parameters);
  jacY = odeSensitivityImplicitStateJacobian(odefun, t, y, yp, parameters, residual);
  jacYp = odeSensitivityImplicitSlopeJacobian(odefun, t, y, yp, parameters, residual);
  jacP = odeSensitivityImplicitParameterJacobian(...
    odefun, t, y, yp, parameters, indices, residual);
  sp0 = zeros(n, p);
  for k = 1:p
    sp0(:, k) = odeSensitivitySolveMass(jacYp, -(jacY * s0(:, k) + jacP(:, k)));
  end
end
%=============================================================================
function sp0 = odeSensitivityInitialSlopeValue(value, n, p)
  if isequal(size(value), [n p])
    sp0 = value;
  elseif numel(value) == n * p
    sp0 = reshape(value, n, p);
  else
    error('Nelson:ode_solvers:sensitivityInitialSlopeSizeMismatch', 'Sensitivity initial slope has incompatible size.');
  end
end
%=============================================================================
function residual = odeSensitivityImplicitResidual(...
    odefun, t, z, zp, n, p, parameters, indices, options)
  y = z(1:n);
  yp = zp(1:n);
  smat = reshape(z(n + 1:length(z)), n, p);
  spmat = reshape(zp(n + 1:length(zp)), n, p);
  f0 = odeSensitivityImplicitPhysicalResidual(odefun, t, y, yp, parameters);
  jacY = odeSensitivityImplicitStateJacobian(odefun, t, y, yp, parameters, f0);
  jacYp = odeSensitivityImplicitSlopeJacobian(odefun, t, y, yp, parameters, f0);
  jacP = odeSensitivityImplicitParameterJacobian(odefun, t, y, yp, parameters, indices, f0);
  sres = jacY * smat + jacYp * spmat + jacP;
  residual = [f0(:); sres(:)];
end
%=============================================================================
function tf = odeSensitivityHasCallbackEvent(eventDefinition)
  response = eventDefinition.Response;
  if iscell(response)
    tf = false;
    for k = 1:numel(response)
      tf = tf || strcmpi(response{k}, 'callback');
    end
  else
    tf = strcmpi(response, 'callback');
  end
end
%=============================================================================
function indices = odeSensitivityParameterIndices(sensitivity, count)
  if isempty(sensitivity.ParameterIndices)
    indices = (1:count).';
  else
    indices = sensitivity.ParameterIndices(:);
  end
  if any(indices > count)
    error('Nelson:ode_solvers:parameterindicesExceedNumberParameters', 'ParameterIndices exceed the number of parameters.');
  end
end
%=============================================================================
function s0 = odeSensitivityInitialValue(sensitivity, n, p)
  if isempty(sensitivity.InitialValue)
    s0 = zeros(n, p);
    return
  end
  value = sensitivity.InitialValue;
  if isequal(size(value), [n p])
    s0 = value;
  elseif numel(value) == n * p
    s0 = reshape(value, n, p);
  else
    error('Nelson:ode_solvers:sensitivityInitialValueIncompatibleSize', 'Sensitivity initial value has incompatible size.');
  end
end
%=============================================================================
function options = odeSensitivityOptions(options, n)
  baseOptions = options;
  options.Parameters = [];
  if ~isempty(baseOptions.Events)
    options.Events = @(t, z) odeSensitivityEvents(baseOptions, t, z, n);
  else
    options.Events = [];
  end
  if isfield(options, 'EventDefinitionObject')
    options = rmfield(options, 'EventDefinitionObject');
  end
  if ~isempty(baseOptions.OutputFcn)
    options.OutputFcn = @(t, z, flag) odeSensitivityOutput(baseOptions, t, z, flag, n);
  else
    options.OutputFcn = [];
  end
  options.OutputSel = [];
  options.NonNegative = baseOptions.NonNegative;
  options.Mass = [];
  options.MvPattern = [];
  options.Jacobian = [];
  options.JPattern = [];
  options.Vectorized = 'off';
end
%=============================================================================
function stop = odeSensitivityOutput(options, t, z, flag, n)
  y = z;
  if ~isempty(z) && isempty(flag)
    y = z(1:n, :);
  elseif ~isempty(z) && strcmp(flag, 'init')
    y = z(1:n, :);
  end
  if ~isempty(options.OutputSel) && (isempty(flag) || strcmp(flag, 'init'))
    y = y(options.OutputSel, :);
  end
  args = {t, y, flag};
  if isfield(options, 'Parameters') && ~isempty(options.Parameters)
    args = odeAppendParameters(args, options.Parameters);
  end
  stop = feval(options.OutputFcn, args{:});
end
%=============================================================================
function [value, isterminal, direction] = odeSensitivityEvents(options, t, z, n)
  y = z(1:n);
  if isfield(options, 'EventDefinitionObject')
    args = {options.EventDefinitionObject, t, y};
    if isfield(options, 'Parameters') && ~isempty(options.Parameters)
      args = odeAppendParameters(args, options.Parameters);
    end
    [value, isterminal, direction] = odeEvaluateEventObject(args{:});
  else
    [value, isterminal, direction] = odeCallFcn(options.Events, t, y, options);
  end
end
%=============================================================================
function dz = odeSensitivityRhs(odefun, t, z, n, parameters, indices, sensitivity, options)
  y = z(1:n);
  pcount = length(indices);
  smat = reshape(z(n + 1:length(z)), n, pcount);
  f0 = odeSensitivityPhysicalRhs(odefun, t, y, parameters, options);
  jacY = odeSensitivityStateJacobian(odefun, t, y, parameters, f0, sensitivity, options);
  jacP = odeSensitivityParameterJacobian(odefun, t, y, parameters, indices, f0, options);
  ds = jacY * smat + jacP;
  dz = [f0(:); ds(:)];
end
%=============================================================================
function residual = odeSensitivityImplicitPhysicalResidual(odefun, t, y, yp, parameters)
  residual = feval(odefun, t, y, yp, parameters);
  residual = residual(:);
end
%=============================================================================
function rhs = odeSensitivityPhysicalRhs(odefun, t, y, parameters, options)
  rhs = feval(odefun, t, y, parameters);
  if ~isempty(options.Mass)
    mass = odeSensitivityMassValue(options.Mass, t, y, parameters, options);
    rhs = odeSensitivitySolveMass(mass, rhs);
  end
end
%=============================================================================
function mass = odeSensitivityMassValue(mass, t, y, parameters, options)
  if isa(mass, 'function_handle')
    localOptions = options;
    localOptions.Parameters = parameters;
    mass = odeCallFcn(mass, t, y, localOptions);
  end
end
%=============================================================================
function rhs = odeSensitivitySolveMass(mass, rhs)
  if isscalar(mass)
    rhs = rhs ./ mass;
  else
    rhs = mass \ rhs;
  end
end
%=============================================================================
function jac = odeSensitivityImplicitStateJacobian(odefun, t, y, yp, parameters, f0)
  n = length(y);
  jac = zeros(n, n);
  for k = 1:n
    step = sqrt(eps) * max(abs(y(k)), 1);
    trial = y;
    trial(k) = trial(k) + step;
    trialResidual = odeSensitivityImplicitPhysicalResidual(odefun, t, trial, yp, parameters);
    jac(:, k) = (trialResidual - f0) / step;
  end
end
%=============================================================================
function jac = odeSensitivityImplicitSlopeJacobian(odefun, t, y, yp, parameters, f0)
  n = length(yp);
  jac = zeros(n, n);
  for k = 1:n
    step = sqrt(eps) * max(abs(yp(k)), 1);
    trial = yp;
    trial(k) = trial(k) + step;
    trialResidual = odeSensitivityImplicitPhysicalResidual(odefun, t, y, trial, parameters);
    jac(:, k) = (trialResidual - f0) / step;
  end
end
%=============================================================================
function jac = odeSensitivityImplicitParameterJacobian(...
    odefun, t, y, yp, parameters, indices, f0)
  n = length(y);
  jac = zeros(n, length(indices));
  for k = 1:length(indices)
    idx = indices(k);
    step = sqrt(eps) * max(abs(parameters(idx)), 1);
    trial = parameters;
    trial(idx) = trial(idx) + step;
    trialResidual = odeSensitivityImplicitPhysicalResidual(odefun, t, y, yp, trial);
    jac(:, k) = (trialResidual - f0) / step;
  end
end
%=============================================================================
function jac = odeSensitivityStateJacobian(odefun, t, y, parameters, f0, sensitivity, options)
  if ~isempty(sensitivity.Jacobian)
    jac = sensitivity.Jacobian.Jacobian;
    if isa(jac, 'function_handle')
      jac = feval(jac, t, y, parameters);
    end
    if ~isempty(options.Mass)
      mass = odeSensitivityMassValue(options.Mass, t, y, parameters, options);
      jac = odeSensitivitySolveMass(mass, jac);
    end
    return
  end
  n = length(y);
  jac = zeros(n, n);
  for k = 1:n
    step = sqrt(eps) * max(abs(y(k)), 1);
    trial = y;
    trial(k) = trial(k) + step;
    trialRhs = odeSensitivityPhysicalRhs(odefun, t, trial, parameters, options);
    jac(:, k) = (trialRhs - f0) / step;
  end
end
%=============================================================================
function jac = odeSensitivityParameterJacobian(odefun, t, y, parameters, indices, f0, options)
  n = length(y);
  jac = zeros(n, length(indices));
  for k = 1:length(indices)
    idx = indices(k);
    step = sqrt(eps) * max(abs(parameters(idx)), 1);
    trial = parameters;
    trial(idx) = trial(idx) + step;
    trialRhs = odeSensitivityPhysicalRhs(odefun, t, y, trial, options);
    jac(:, k) = (trialRhs - f0) / step;
  end
end
%=============================================================================
function sol = odeSensitivitySplitSolution(sol, n, p)
  z = sol.y;
  nt = size(z, 2);
  sol.y = z(1:n, :);
  if isfield(sol, 'extdata') && isfield(sol.extdata, 'slopes')
    sol.extdata.slopes = sol.extdata.slopes(1:n, :);
  end
  if isfield(sol, 'extdata') && isfield(sol.extdata, 'yp0') && ~isempty(sol.extdata.yp0)
    sol.extdata.yp0 = sol.extdata.yp0(1:n);
  end
  sensitivity = zeros(n, p, nt);
  for k = 1:nt
    sensitivity(:, :, k) = reshape(z(n + 1:size(z, 1), k), n, p);
  end
  sol.sensitivity = sensitivity;
  if isfield(sol, 'ye')
    zevent = sol.ye;
    sol.ye = zevent(1:n, :);
    ne = size(zevent, 2);
    eventSensitivity = zeros(n, p, ne);
    for k = 1:ne
      eventSensitivity(:, :, k) = reshape(zevent(n + 1:size(zevent, 1), k), n, p);
    end
    sol.eventSensitivity = eventSensitivity;
  end
end
%=============================================================================
