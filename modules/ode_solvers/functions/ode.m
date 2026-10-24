%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ode
  properties (Hidden)
    IsConstructed = false
  end

  properties
    EquationType = 'standard'
    ODEFcn = []
    Parameters = []
    InitialTime = 0
    InitialValue = zeros(0, 1)
    InitialSlope = zeros(0, 1)
    Jacobian = []
    MassMatrix = []
    NonNegativeVariables = []
    EventDefinition = []
    DelayDefinition = []
    Sensitivity = []
    AbsoluteTolerance = 1e-6
    RelativeTolerance = 1e-3
    Solver = 'auto'
    SolverOptions = nelson.ode.options.ODE45()
    SeparateComplexParts = 'off'
  end

  properties (SetAccess = protected)
    SelectedSolver = 'ode45'
  end

  properties (Hidden)
    Version = 1
  end

  methods
    function obj = ode(varargin)
      if mod(nargin, 2) ~= 0
        error('Nelson:ode_solvers:nameValueArgumentsExpected', 'Name-value arguments expected.');
      end
      hasExplicitSolver = false;
      hasExplicitSolverOptions = false;
      explicitSolver = '';
      explicitSolverOptions = [];
      for k = 1:2:nargin
        name = varargin{k};
        if isstring(name)
          name = char(name);
        end
        if ischar(name)
          if strcmpi(name, 'Solver')
            hasExplicitSolver = true;
            explicitSolver = odeSolverName(varargin{k + 1});
          elseif strcmpi(name, 'SolverOptions')
            hasExplicitSolverOptions = true;
            explicitSolverOptions = varargin{k + 1};
          end
        end
      end
      if hasExplicitSolver && hasExplicitSolverOptions ...
          && odeIsObjectSolverOptions(explicitSolverOptions)
        odeIncompatibleSolverOptions(explicitSolver, explicitSolverOptions.ID);
      end
      for k = 1:2:nargin
        obj = assignProperty(obj, varargin{k}, varargin{k + 1}, false);
      end
      obj = odeEnsureObjectDefaults(obj, hasExplicitSolver, hasExplicitSolverOptions);
      obj = odeSelectSolver(obj);
      obj.IsConstructed = true;
    end

    function obj = set.ODEFcn(obj, value)
      if ~isempty(value) && ~isa(value, 'function_handle')
        error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
      end
      obj.ODEFcn = value;
    end

    function obj = set.EquationType(obj, value)
      obj.EquationType = odeValidateEquationType(value);
      obj = odeSelectSolver(obj);
    end

    function obj = set.SeparateComplexParts(obj, value)
      obj.SeparateComplexParts = odeValidateOnOffObject(value, 'SeparateComplexParts');
    end

    function obj = set.InitialValue(obj, value)
      odeValidateVector(value, 'InitialValue');
      obj.InitialValue = value(:);
    end

    function obj = set.InitialTime(obj, value)
      odeValidateTimeScalar(value, 'InitialTime');
      obj.InitialTime = value;
    end

    function obj = set.InitialSlope(obj, value)
      odeValidateVector(value, 'InitialSlope');
      obj.InitialSlope = value(:);
    end

    function obj = set.RelativeTolerance(obj, value)
      odeValidateTolerance(value, 'RelativeTolerance');
      obj.RelativeTolerance = value;
    end

    function obj = set.AbsoluteTolerance(obj, value)
      odeValidateTolerance(value, 'AbsoluteTolerance');
      obj.AbsoluteTolerance = value;
    end

    function obj = set.Solver(obj, value)
      solver = odeSolverName(value);
      obj.Solver = solver;
      if obj.IsConstructed
        obj = odeSelectSolver(obj, solver);
      end
    end

    function obj = set.SolverOptions(obj, value)
      if obj.IsConstructed && odeIsSolverAlias(obj.Solver)
        odeSolverOptionsAliasError(obj.Solver);
      end
      if ~odeIsObjectSolverOptions(value)
        error('Nelson:ode_solvers:solverOptionsObjectExpected', 'Solver options object expected.');
      end
      obj.SolverOptions = value;
      obj.Solver = value.ID;
      obj.SelectedSolver = value.ID;
    end

    function obj = set.MassMatrix(obj, value)
      if isempty(value) || isa(value, 'odeMassMatrix')
        obj.MassMatrix = value;
      else
        obj.MassMatrix = odeMassMatrix(value);
      end
    end

    function obj = set.Jacobian(obj, value)
      if isempty(value) || isa(value, 'odeJacobian')
        obj.Jacobian = value;
      else
        obj.Jacobian = odeJacobian(value);
      end
    end

    function obj = set.EventDefinition(obj, value)
      if isempty(value) || isa(value, 'odeEvent')
        obj.EventDefinition = value;
      else
        obj.EventDefinition = odeEvent(value);
      end
    end

    function obj = set.DelayDefinition(obj, value)
      if isempty(value) || isa(value, 'odeDelay')
        obj.DelayDefinition = value;
      else
        error('Nelson:ode_solvers:delayDefinitionObjectExpected', '%s definition object expected.', 'Delay');
      end
    end

    function obj = set.Sensitivity(obj, value)
      if isempty(value) || isa(value, 'odeSensitivity')
        obj.Sensitivity = value;
      else
        error('Nelson:ode_solvers:sensitivityDefinitionObjectExpected', '%s definition object expected.', 'Sensitivity');
      end
    end

    function result = solve(obj, varargin)
      narginchk(2, 3);
      obj = odeSelectSolver(obj);
      odeCheckDeferred(obj);
      tspan = odeObjectTimeSpan(obj, varargin{:});
      options = odeOptionsFromObject(obj);
      odefun = obj.ODEFcn;
      initialValue = obj.InitialValue;
      initialSlope = obj.InitialSlope;
      restore = [];
      if odeHasDelayDefinition(obj.DelayDefinition)
        if ~isempty(obj.Sensitivity)
          if strcmpi(obj.SeparateComplexParts, 'on')
            error('Nelson:ode_solvers:sensitivityEquationsSeparatedComplexParts', '%s equations with separated complex parts are not supported yet.', 'Sensitivity');
          end
          sol = odeRunDelaySensitivity( ...
            obj.SelectedSolver, odefun, tspan, initialValue, initialSlope, options, ...
            obj.DelayDefinition, obj.Sensitivity);
          result = nelson.ode.ODEResults(sol);
          return
        end
        if strcmpi(obj.SeparateComplexParts, 'on')
          error('Nelson:ode_solvers:delayEquationsSeparatedComplexParts', '%s equations with separated complex parts are not supported yet.', 'Delay');
        end
        sol = odeRunDelaySolver( ...
          obj.SelectedSolver, odefun, tspan, initialValue, initialSlope, options, obj.DelayDefinition);
        result = nelson.ode.ODEResults(sol);
        return
      end
      if odeShouldSeparateComplexParts(obj, initialValue, initialSlope, options)
        [odefun, initialValue, initialSlope, options, restore] = ...
          odeSeparateComplexParts(obj.SelectedSolver, odefun, initialValue, initialSlope, options);
      end
      [initialValue, initialSlope, options] = odeApplyConsistentInitialConditions( ...
        obj.SelectedSolver, odefun, tspan(1), initialValue, initialSlope, options);
      if ~isempty(obj.Sensitivity)
        if ~isempty(restore)
          error('Nelson:ode_solvers:sensitivityEquationsSeparatedComplexParts', '%s equations with separated complex parts are not supported yet.', 'Sensitivity');
        end
        sol = odeRunSensitivity( ...
          obj.SelectedSolver, odefun, tspan, initialValue, initialSlope, options, obj.Sensitivity);
        result = nelson.ode.ODEResults(sol);
        return
      end
      sol = odeRunSolver(obj.SelectedSolver, odefun, tspan, initialValue, initialSlope, options);
      if ~isempty(restore)
        sol = odeRestoreComplexSolution(sol, restore);
      end
      result = nelson.ode.ODEResults(sol);
    end

    function [f, data] = solutionFcn(obj, varargin)
      narginchk(2, 3);
      result = solve(obj, varargin{:});
      data = result;
      f = @(t) deval(result, t);
    end

    function s = saveobj(obj)
      s = struct(obj);
      if isfield(s, 'IsConstructed')
        s = rmfield(s, 'IsConstructed');
      end
    end

    function obj = assignProperty(obj, name, value, allowInternal)
      if nargin < 4
        allowInternal = false;
      end
      if isstring(name)
        name = char(name);
      end
      switch lower(name)
        case 'odefcn'
          if ~isempty(value) && ~isa(value, 'function_handle')
            error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
          end
          obj.ODEFcn = value;
        case 'equationtype'
          obj.EquationType = odeValidateEquationType(value);
        case 'initialvalue'
          odeValidateVector(value, 'InitialValue');
          obj.InitialValue = value(:);
        case 'initialtime'
          odeValidateTimeScalar(value, 'InitialTime');
          obj.InitialTime = value;
        case 'initialslope'
          odeValidateVector(value, 'InitialSlope');
          obj.InitialSlope = value(:);
        case 'relativetolerance'
          odeValidateTolerance(value, 'RelativeTolerance');
          obj.RelativeTolerance = value;
        case 'absolutetolerance'
          odeValidateTolerance(value, 'AbsoluteTolerance');
          obj.AbsoluteTolerance = value;
        case 'solver'
          solver = odeSolverName(value);
          obj.Solver = solver;
          obj = odeSelectSolver(obj, solver);
        case 'separatecomplexparts'
          obj.SeparateComplexParts = odeValidateOnOffObject(value, 'SeparateComplexParts');
        case 'solveroptions'
          if ~odeIsObjectSolverOptions(value)
            error('Nelson:ode_solvers:solverOptionsObjectExpected', 'Solver options object expected.');
          end
          obj.Solver = value.ID;
          obj.SelectedSolver = value.ID;
          obj.SolverOptions = value;
        case 'massmatrix'
          if isempty(value) || isa(value, 'odeMassMatrix')
            obj.MassMatrix = value;
          else
            obj.MassMatrix = odeMassMatrix(value);
          end
        case 'jacobian'
          if isempty(value) || isa(value, 'odeJacobian')
            obj.Jacobian = value;
          else
            obj.Jacobian = odeJacobian(value);
          end
        case 'eventdefinition'
          if isempty(value) || isa(value, 'odeEvent')
            obj.EventDefinition = value;
          else
            obj.EventDefinition = odeEvent(value);
          end
        case 'parameters'
          obj.Parameters = value;
        case 'nonnegativevariables'
          obj.NonNegativeVariables = value;
        case 'delaydefinition'
          if isempty(value) || isa(value, 'odeDelay')
            obj.DelayDefinition = value;
          else
            error('Nelson:ode_solvers:delayDefinitionObjectExpected', '%s definition object expected.', 'Delay');
          end
        case 'sensitivity'
          if isempty(value) || isa(value, 'odeSensitivity')
            obj.Sensitivity = value;
          else
            error('Nelson:ode_solvers:sensitivityDefinitionObjectExpected', '%s definition object expected.', 'Sensitivity');
          end
        case 'selectedsolver'
          if allowInternal
            obj.SelectedSolver = value;
          else
            odeInvalidArgumentName(name);
          end
        case 'version'
          if allowInternal
            obj.Version = value;
          else
            odeInvalidArgumentName(name);
          end
        otherwise
          odeInvalidArgumentName(name);
      end
    end
  end

  methods (Static)
    function obj = loadobj(s)
      obj = ode();
      names = fieldnames(s);
      for k = 1:length(names)
        if strcmp(names{k}, 'IsConstructed')
          continue
        end
        obj = assignProperty(obj, names{k}, s.(names{k}), true);
      end
      obj = odeSelectSolver(obj);
      obj.IsConstructed = true;
    end
  end
end
%=============================================================================
function tspan = odeObjectTimeSpan(obj, varargin)
  if length(varargin) == 1
    odeValidateTimeScalar(varargin{1}, 'FinalTime');
    tspan = [obj.InitialTime varargin{1}];
  else
    odeValidateTimeScalar(varargin{1}, 'InitialTime');
    odeValidateTimeScalar(varargin{2}, 'FinalTime');
    tspan = [varargin{1} varargin{2}];
  end
end
%=============================================================================
function obj = odeEnsureObjectDefaults(obj, hasExplicitSolver, hasExplicitSolverOptions)
  if nargin < 2
    hasExplicitSolver = false;
    hasExplicitSolverOptions = false;
  end
  if ~hasExplicitSolver && ~hasExplicitSolverOptions
    obj.Solver = 'auto';
  elseif isempty(obj.Solver)
    obj.Solver = 'auto';
  end
  if ~odeIsObjectSolverOptions(obj.SolverOptions)
    obj.SolverOptions = nelson.ode.options.ODE45();
  end
end
%=============================================================================
function tf = odeIsObjectSolverOptions(value)
  tf = isa(value, 'nelson.ode.Options');
end
%=============================================================================
function odeIncompatibleSolverOptions(solver, optionsSolver)
  error('Nelson:ode_solvers:incompatibleSolverOptions', 'Specified solver (%s) is incompatible with specified options (%s). Specify one or the other.', solver, optionsSolver);
end
%=============================================================================
function tf = odeIsSolverAlias(solver)
  tf = strcmp(solver, 'auto') || strcmp(solver, 'stiff') || strcmp(solver, 'nonstiff');
end
%=============================================================================
function odeSolverOptionsAliasError(solver)
  error('Nelson:ode_solvers:invalidSolverOptions', 'SolverOptions property cannot be modified when the Solver property is "%s". Set the Solver property to "ode45" and try again.', solver);
end
%=============================================================================
function odeInvalidArgumentName(name)
  error('Nelson:ode_solvers:invalidArgumentName', 'Invalid argument name ''%s''.', name);
end
%=============================================================================
function odeValidateTimeScalar(value, name)
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value)
    error('Nelson:ode_solvers:mustBeFiniteScalar', '%s must be a finite scalar.', name);
  end
end
%=============================================================================
function value = odeValidateEquationType(value)
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    error('Nelson:ode_solvers:invalidValue', '%s has an invalid value.', 'EquationType');
  end
  if strcmpi(value, 'standard')
    value = 'standard';
  elseif strcmpi(value, 'fullyimplicit')
    value = 'fullyimplicit';
  else
    error('Nelson:ode_solvers:invalidValue', '%s has an invalid value.', 'EquationType');
  end
end
%=============================================================================
function value = odeValidateOnOffObject(value, name)
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value) || ~(strcmpi(value, 'on') || strcmpi(value, 'off'))
    error('Nelson:ode_solvers:invalidValue:valueOff', '%s must be ''on'' or ''off''.', name);
  end
  if strcmpi(value, 'on')
    value = 'on';
  else
    value = 'off';
  end
end
%=============================================================================
function tf = odeShouldSeparateComplexParts(obj, initialValue, initialSlope, options)
  if strcmpi(obj.SeparateComplexParts, 'on')
    tf = true;
    return
  end
  tf = odeIsSundialsSolver(obj.SelectedSolver) ...
    && odeProblemHasComplexParts(initialValue, initialSlope, options);
end
%=============================================================================
function tf = odeProblemHasComplexParts(initialValue, initialSlope, options)
  tf = ~isreal(initialValue) || ~isreal(initialSlope) ...
    || odeOptionHasComplexParts(options.Mass) ...
    || odeOptionHasComplexParts(options.Jacobian);
end
%=============================================================================
function tf = odeOptionHasComplexParts(value)
  tf = isnumeric(value) && ~isreal(value);
end
%=============================================================================
