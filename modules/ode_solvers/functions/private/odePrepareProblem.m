%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function problem = odePrepareProblem(solver, odefun, tspan, y0, yp0, options)
  if ischar(odefun) || isStringScalar(odefun)
    odefun = odeLegacyTextFunction(str2func(char(odefun)), solver);
  end
  if ~isa(odefun, 'function_handle')
    error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
  end
  if ~isnumeric(tspan) || numel(tspan) < 2
    error('Nelson:ode_solvers:timeSpanContainLeastTwoValues', 'Time span must contain at least two values.');
  end
  if ~isvector(tspan) || any(~isfinite(tspan(:))) || ~odeIsStrictlyMonotone(tspan(:).')
    error('Nelson:ode_solvers:timeSpanEntriesStrictlyIncreaseDecrease', 'Time span entries must strictly increase or decrease.');
  end
  tspan = tspan(:).';
  odeValidateVector(y0, 'InitialValue');
  if nargin < 6 || isempty(options)
    options = odeDefaultOptions();
  elseif odeIsSolverOptions(options)
    options = struct(options);
  end
  parameters = [];
  eventDefinitionObject = [];
  if isstruct(options) && isfield(options, 'Parameters')
    parameters = options.Parameters;
    options = rmfield(options, 'Parameters');
  end
  if isstruct(options) && isfield(options, 'EventDefinitionObject')
    eventDefinitionObject = options.EventDefinitionObject;
    options = rmfield(options, 'EventDefinitionObject');
  end
  options = odeMergeOptions(odeDefaultOptions(), options);
  options.Parameters = parameters;
  options = odeApplyDefaults(options, tspan, y0, solver);
  if ~isempty(eventDefinitionObject)
    options.EventDefinitionObject = eventDefinitionObject;
  end
  tfinal = tspan(length(tspan));
  problem.solver = solver;
  problem.odefun = odefun;
  problem.t0 = tspan(1);
  problem.tfinal = tfinal;
  problem.tspan = tspan;
  problem.y0 = y0(:);
  problem.yp0 = yp0;
  problem.options = options;
  problem.direction = sign(problem.tfinal - problem.t0);
  problem.errorExponent = odeErrorExponent(solver);
  if problem.direction == 0
    error('Nelson:ode_solvers:timeSpanEndpointsDistinct', 'Time span endpoints must be distinct.');
  end
end
%=============================================================================
function fcn = odeLegacyTextFunction(originalFcn, solver)
  isImplicit = strcmp(solver, 'ode15i');
  fcn = @(t, y, varargin) odeCallLegacyTextFunction(originalFcn, isImplicit, t, y, varargin{:});
end
%=============================================================================
function varargout = odeCallLegacyTextFunction(originalFcn, isImplicit, t, y, varargin)
  if isImplicit
    args = [{t, y, varargin{1}, []}, varargin(2:end)];
  else
    args = [{t, y, []}, varargin];
  end
  if nargout == 0
    feval(originalFcn, args{:});
  else
    [varargout{1:nargout}] = feval(originalFcn, args{:});
  end
end
%=============================================================================
function tf = odeIsStrictlyMonotone(tspan)
  delta = diff(tspan);
  tf = all(delta > 0) || all(delta < 0);
end
%=============================================================================
function exponent = odeErrorExponent(solver)
  if strcmp(solver, 'ode23')
    exponent = -1 / 3;
  elseif strcmp(solver, 'ode113')
    exponent = -1 / 4;
  elseif strcmp(solver, 'ode78')
    exponent = -1 / 7;
  elseif strcmp(solver, 'ode89')
    exponent = -1 / 8;
  elseif strcmp(solver, 'ode15s') || strcmp(solver, 'ode23tb')
    exponent = -1 / 3;
  elseif strcmp(solver, 'ode23s') || strcmp(solver, 'ode23t')
    exponent = -1 / 2;
  else
    exponent = -1 / 5;
  end
end
%=============================================================================
