%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef Options
  properties (Hidden)
    ID = 'ode45'
    DefaultRefine = 1
    Refine = []
  end

  methods
    function obj = set.Refine(obj, value)
      if isempty(value)
        obj.Refine = [];
        return
      end
      if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value) || value <= 0 ...
          || value ~= fix(value)
        error('Nelson:ode_solvers:refineIntegerScalar', 'Refine must be an integer scalar.');
      end
      obj.Refine = value;
    end

    function tf = isDefault(obj)
      names = odeOptionPublicProperties(obj);
      tf = true(size(obj));
      for k = 1:length(names)
        value = obj.(names{k});
        tf = tf && odeOptionValueIsDefault(names{k}, value);
      end
    end

    function s = struct(obj)
      s = odeDefaultOptionStruct();
      names = odeOptionPublicProperties(obj);
      for k = 1:length(names)
        name = odeOptionStructName(names{k});
        s.(name) = obj.(names{k});
      end
      if isempty(obj.Refine)
        s.Refine = obj.DefaultRefine;
      else
        s.Refine = obj.Refine;
      end
      s.ID = obj.ID;
    end
  end

  methods (Static)
    function obj = makeSolverOptions(solver, varargin)
      solver = odeSolverNameForOptions(solver);
      switch solver
        case 'ode23'
          obj = nelson.ode.options.ODE23(varargin{:});
        case 'ode78'
          obj = nelson.ode.options.ODE78(varargin{:});
        case 'ode89'
          obj = nelson.ode.options.ODE89(varargin{:});
        case 'ode113'
          obj = nelson.ode.options.ODE113(varargin{:});
        case 'ode15s'
          obj = nelson.ode.options.ODE15s(varargin{:});
        case 'ode23s'
          obj = nelson.ode.options.ODE23s(varargin{:});
        case 'ode23t'
          obj = nelson.ode.options.ODE23t(varargin{:});
        case 'ode23tb'
          obj = nelson.ode.options.ODE23tb(varargin{:});
        case 'ode15i'
          obj = nelson.ode.options.ODE15i(varargin{:});
        case {'autoswitch', 'cvodesnonstiff'}
          obj = nelson.ode.options.CVODESNonstiff(varargin{:});
        case 'cvodesstiff'
          obj = nelson.ode.options.CVODESStiff(varargin{:});
        case 'idas'
          obj = nelson.ode.options.IDAS(varargin{:});
        otherwise
          obj = nelson.ode.options.ODE45(varargin{:});
      end
    end
  end
end
%=============================================================================
function names = odeOptionPublicProperties(obj)
  names = properties(obj);
  hidden = {'ID'; 'DefaultRefine'; 'Refine'};
  keep = true(size(names));
  for k = 1:length(hidden)
    keep = keep & ~strcmp(names, hidden{k});
  end
  names = names(keep);
end
%=============================================================================
function name = odeOptionStructName(name)
  if strcmp(name, 'OutputSelection')
    name = 'OutputSel';
  elseif strcmp(name, 'Vectorization')
    name = 'Vectorized';
  end
end
%=============================================================================
function tf = odeOptionValueIsDefault(name, value)
  switch name
    case {'InitialStep', 'MaxStep', 'MinStep', 'OutputFcn', 'OutputSelection'}
      tf = isempty(value);
    case 'NormControl'
      tf = strcmpi(char(value), 'off');
    case 'Vectorization'
      if iscell(value)
        tf = strcmpi(char(value{1}), 'off') && strcmpi(char(value{2}), 'off');
      else
        tf = strcmpi(char(value), 'off');
      end
    case 'BDF'
      tf = strcmpi(char(value), 'off');
    case 'MaxOrder'
      tf = isequal(value, 5);
    case 'ComputeConsistentInitialConditions'
      tf = isequal(value, true);
    case {'LinearSolver', 'Preconditioner'}
      tf = isempty(value) || strcmpi(char(value), 'auto');
    otherwise
      tf = isempty(value);
  end
end
%=============================================================================
function s = odeDefaultOptionStruct()
  s = struct( ...
    'AbsTol', [], ...
    'BDF', [], ...
    'Events', [], ...
    'InitialStep', [], ...
    'Jacobian', [], ...
    'JConstant', [], ...
    'JPattern', [], ...
    'LinearSolver', [], ...
    'Mass', [], ...
    'MassSingular', [], ...
    'MaxOrder', [], ...
    'MaxStep', [], ...
    'MinStep', [], ...
    'NonNegative', [], ...
    'NormControl', [], ...
    'OutputFcn', [], ...
    'OutputSel', [], ...
    'Refine', [], ...
    'RelTol', [], ...
    'Stats', [], ...
    'Preconditioner', [], ...
    'Vectorized', [], ...
    'MStateDependence', [], ...
    'MvPattern', [], ...
    'InitialSlope', []);
end
%=============================================================================
function solver = odeSolverNameForOptions(solver)
  if isstring(solver)
    solver = char(solver);
  end
  if ~ischar(solver)
    error('Nelson:ode_solvers:unknownOdeSolver', 'Unknown ODE solver.');
  end
  solver = lower(solver);
  switch solver
    case {'autoswitch', 'ode45', 'ode23', 'ode78', 'ode89', 'ode113', 'ode15s', ...
        'ode23s', 'ode23t', 'ode23tb', 'ode15i', 'cvodesnonstiff', ...
        'cvodesstiff', 'idas'}
      return
    otherwise
      error('Nelson:ode_solvers:unknownOdeSolver', 'Unknown ODE solver.');
  end
end
%=============================================================================
