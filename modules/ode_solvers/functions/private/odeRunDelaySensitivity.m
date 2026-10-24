%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = odeRunDelaySensitivity(solver, odefun, tspan, y0, yp0, options, delay, sensitivity)
  odeValidateDelaySensitivity(options, sensitivity);
  if strcmp(sensitivity.Method, 'adjoint')
    sol = odeRunDelayProjectedAdjoint(solver, odefun, tspan, y0, yp0, options, delay, sensitivity);
    return
  end
  sol = odeRunDelayDirectSensitivity(solver, odefun, tspan, y0, yp0, options, delay, sensitivity);
end
%=============================================================================
function sol = odeRunDelayDirectSensitivity(solver, odefun, tspan, y0, yp0, options, delay, sensitivity)
  parameters = options.Parameters(:);
  indices = odeDelaySensitivityParameterIndices(sensitivity, length(parameters));
  baseSol = odeRunDelaySolver(solver, odefun, tspan, y0, yp0, options, delay);
  n = size(baseSol.y, 1);
  p = length(indices);
  s0 = odeDelaySensitivityInitialValue(sensitivity, n, p);
  values = zeros(n, p, length(baseSol.x));
  eventValues = odeDelaySensitivityEventStorage(baseSol, n, p);
  for k = 1:p
    idx = indices(k);
    step = sqrt(eps) * max(abs(parameters(idx)), 1);
    trialParameters = parameters;
    trialParameters(idx) = trialParameters(idx) + step;
    trialOptions = options;
    trialOptions.Parameters = trialParameters;
    trialY0 = y0(:) + step * s0(:, k);
    trialSol = odeRunDelaySolver(solver, odefun, tspan, trialY0, yp0, trialOptions, delay);
    values(:, k, :) = (odeDelaySensitivityValuesAt(trialSol, baseSol.x) - baseSol.y) / step;
    eventValues = odeDelaySensitivityEvents(eventValues, baseSol, trialSol, k, step);
  end
  sol = baseSol;
  sol.sensitivity = values;
  if ~isempty(eventValues)
    sol.eventSensitivity = eventValues;
  end
end
%=============================================================================
function sol = odeRunDelayProjectedAdjoint(solver, odefun, tspan, y0, yp0, options, delay, sensitivity)
  directSensitivity = sensitivity;
  directSensitivity.Method = 'direct';
  directSol = odeRunDelayDirectSensitivity(...
    solver, odefun, tspan, y0, yp0, options, delay, directSensitivity);
  parameters = options.Parameters(:);
  indices = odeDelaySensitivityParameterIndices(sensitivity, length(parameters));
  gradient = odeProjectedAdjointGradient(directSol, sensitivity, parameters, indices);
  sol = directSol;
  if isfield(sol, 'sensitivity')
    sol = rmfield(sol, 'sensitivity');
  end
  sol.adjointGradient = gradient(:).';
  sol.stats.sensitivityMethod = 'adjoint';
end
%=============================================================================
function odeValidateDelaySensitivity(options, sensitivity)
  if strcmp(sensitivity.Method, 'adjoint')
    if isempty(sensitivity.ObjectiveFcn) && isempty(sensitivity.QuadratureFcn)
      error('Nelson:ode_solvers:adjointSensitivitiesRequireObjectivefcnQuadraturefcn', 'Adjoint sensitivities require ObjectiveFcn or QuadratureFcn.');
    end
    if ~isempty(sensitivity.InitialValue) || ~isempty(sensitivity.InitialSlope)
      error('Nelson:ode_solvers:adjointSensitivitiesNotUseSensitivityInitial', 'Adjoint sensitivities do not use sensitivity initial conditions.');
    end
  end
  if ~isnumeric(options.Parameters) || ~isreal(options.Parameters) || ~isvector(options.Parameters)
    error('Nelson:ode_solvers:parametersNumericVectorPerformSensitivityAnalysis', 'Parameters must be a numeric vector to perform a sensitivity analysis.');
  end
  if ~isempty(options.OutputFcn)
    error('Nelson:ode_solvers:sensitivityEquationsDelayedOutputFunctions', 'Sensitivity equations with delayed output functions are not supported yet.');
  end
end
%=============================================================================
function indices = odeDelaySensitivityParameterIndices(sensitivity, count)
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
function s0 = odeDelaySensitivityInitialValue(sensitivity, n, p)
  if isempty(sensitivity.InitialValue)
    s0 = zeros(n, p);
  elseif isequal(size(sensitivity.InitialValue), [n p])
    s0 = sensitivity.InitialValue;
  elseif numel(sensitivity.InitialValue) == n * p
    s0 = reshape(sensitivity.InitialValue, n, p);
  else
    error('Nelson:ode_solvers:sensitivityInitialValueIncompatibleSize', 'Sensitivity initial value has incompatible size.');
  end
end
%=============================================================================
function values = odeDelaySensitivityValuesAt(sol, times)
  values = interp1(sol.x(:), sol.y.', times(:), 'linear').';
end
%=============================================================================
function eventValues = odeDelaySensitivityEventStorage(sol, n, p)
  if isfield(sol, 'ye')
    eventValues = zeros(n, p, size(sol.ye, 2));
  else
    eventValues = [];
  end
end
%=============================================================================
function eventValues = odeDelaySensitivityEvents(eventValues, baseSol, trialSol, k, step)
  if isempty(eventValues) || ~isfield(trialSol, 'ye') || ~isequal(size(baseSol.ye), size(trialSol.ye))
    return
  end
  eventValues(:, k, :) = (trialSol.ye - baseSol.ye) / step;
end
%=============================================================================
