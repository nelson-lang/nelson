%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = odeRunAdjointSensitivity(solver, odefun, tspan, y0, yp0, options, sensitivity)
  if ~odeIsSundialsSolver(solver)
    error('Nelson:ode_solvers:adjointSensitivitiesRequireSundialsSolver', 'Adjoint sensitivities require a SUNDIALS solver.');
  end
  if ~odeSundialsAvailable()
    error('Nelson:ode_solvers:sundialsBackendNotAvailable', 'SUNDIALS backend is not available.');
  end
  if ~isempty(options.Events)
    error('Nelson:ode_solvers:adjointEventsUnsupported', 'Adjoint sensitivities with events are not supported yet.');
  end
  if isempty(sensitivity.ObjectiveFcn) && isempty(sensitivity.QuadratureFcn)
    error('Nelson:ode_solvers:adjointSensitivitiesRequireObjectivefcnQuadraturefcn', 'Adjoint sensitivities require ObjectiveFcn or QuadratureFcn.');
  end
  if ~isempty(sensitivity.InitialValue) || ~isempty(sensitivity.InitialSlope)
    error('Nelson:ode_solvers:adjointSensitivitiesNotUseSensitivityInitial', 'Adjoint sensitivities do not use sensitivity initial conditions.');
  end
  if ~isempty(sensitivity.ObjectiveTime) ...
      && abs(sensitivity.ObjectiveTime - tspan(length(tspan))) > ...
      100 * eps * max(1, abs(tspan(length(tspan))))
    error('Nelson:ode_solvers:adjointFinalTimeOnly', 'Only final-time adjoint objectives are supported yet.');
  end
  parameters = options.Parameters(:);
  indices = odeAdjointParameterIndices(sensitivity, length(parameters));
  adjointData = struct();
  adjointData.Method = 'adjoint';
  adjointData.Parameters = parameters(:);
  adjointData.ParameterIndices = indices(:) - 1;
  adjointData.ObjectiveFcn = sensitivity.ObjectiveFcn;
  adjointData.QuadratureFcn = sensitivity.QuadratureFcn;
  adjointData.ObjectiveTime = sensitivity.ObjectiveTime;
  adjointData.RelativeTolerance = sensitivity.AdjointRelativeTolerance;
  adjointData.AbsoluteTolerance = sensitivity.AdjointAbsoluteTolerance;
  nativeOptions = options;
  nativeOptions.Sensitivity = adjointData;
  problem = odePrepareProblem(solver, odefun, tspan, y0, yp0, nativeOptions);
  sol = __ode_sundials_run__(problem);
  odeDisplayStats(problem, sol.stats);
end
function indices = odeAdjointParameterIndices(sensitivity, count)
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
