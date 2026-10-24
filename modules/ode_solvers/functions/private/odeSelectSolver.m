%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = odeSelectSolver(obj, solver)
  if nargin < 2
    solver = obj.Solver;
  end
  if isempty(solver)
    solver = 'auto';
  end
  solver = odeSolverName(solver);
  requestedSolver = solver;
  useSundials = odeCanUseSundialsAuto(obj);
  switch solver
    case 'auto'
      if useSundials && strcmp(obj.EquationType, 'fullyimplicit')
        obj.SelectedSolver = 'idas';
      elseif useSundials && ~isempty(obj.MassMatrix)
        obj.SelectedSolver = 'cvodesstiff';
      elseif useSundials
        obj.SelectedSolver = 'cvodesnonstiff';
      elseif strcmp(obj.EquationType, 'fullyimplicit')
        obj.SelectedSolver = 'ode15i';
      elseif ~isempty(obj.MassMatrix)
        obj.SelectedSolver = 'ode15s';
      else
        obj.SelectedSolver = 'ode45';
      end
    case 'autoswitch'
      if strcmp(obj.EquationType, 'fullyimplicit')
        error('Nelson:ode_solvers:autoSwitchingSupportedExplicitOdeProblems', 'Auto-switching is only supported for explicit ODE problems.');
      end
      if ~isempty(obj.MassMatrix)
        error('Nelson:ode_solvers:autoSwitchingMassMatricesNotSupported', 'Auto-switching with mass matrices is not supported yet.');
      end
      obj.SelectedSolver = 'autoswitch';
    case 'stiff'
      if useSundials
        obj.SelectedSolver = 'cvodesstiff';
      else
        obj.SelectedSolver = 'ode15s';
      end
    case 'nonstiff'
      if useSundials
        obj.SelectedSolver = 'cvodesnonstiff';
      else
        obj.SelectedSolver = 'ode45';
      end
    otherwise
      obj.SelectedSolver = solver;
  end
  if strcmp(obj.SelectedSolver, 'autoswitch')
    obj.SolverOptions = odeMakeSolverOptions('ode45');
    obj.SelectedSolver = 'autoswitch';
    obj.Solver = requestedSolver;
    return
  end
  if ~odeIsSolverOptions(obj.SolverOptions) ...
      || ~strcmp(obj.SolverOptions.ID, obj.SelectedSolver)
    obj.SolverOptions = odeMakeSolverOptions(obj.SelectedSolver);
    obj.Solver = requestedSolver;
  end
end
%=============================================================================
function tf = odeCanUseSundialsAuto(obj)
  tf = odeSundialsAvailable();
  if ~tf
    return
  end
  if odeHasDirectSensitivity(obj) || odeHasDelayDefinition(obj.DelayDefinition) ...
      || ~isempty(obj.Jacobian) || ~isempty(obj.NonNegativeVariables)
    tf = false;
    return
  end
  if odeIsSolverOptions(obj.SolverOptions) && isprop(obj.SolverOptions, 'OutputFcn') ...
      && ~isempty(obj.SolverOptions.OutputFcn)
    tf = false;
    return
  end
  if ~isempty(obj.MassMatrix) && strcmp(obj.MassMatrix.Singular, 'yes')
    tf = false;
    return
  end
  if ~isempty(obj.EventDefinition) && odeEventUsesCallback(obj.EventDefinition)
    tf = false;
  end
end
%=============================================================================
function tf = odeHasDirectSensitivity(obj)
  tf = ~isempty(obj.Sensitivity) && ~strcmp(obj.Sensitivity.Method, 'adjoint');
end
%=============================================================================
function tf = odeEventUsesCallback(eventDefinition)
  responses = eventDefinition.Response;
  if ischar(responses) || isstring(responses)
    tf = strcmpi(char(responses), 'callback');
  elseif iscell(responses)
    tf = false;
    for k = 1:numel(responses)
      tf = tf || strcmpi(char(responses{k}), 'callback');
    end
  else
    tf = false;
  end
end
%=============================================================================
