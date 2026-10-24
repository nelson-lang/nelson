%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = odeApplyDefaults(options, tspan, y0, solver)
  tfinal = tspan(length(tspan));
  odeValidateRawOptions(options, y0, solver);
  if isempty(options.RelTol)
    options.RelTol = 1e-3;
  end
  if isempty(options.AbsTol)
    options.AbsTol = 1e-6;
  end
  if isempty(options.MaxStep)
    options.MaxStep = abs(tfinal - tspan(1)) / 10;
  end
  if isempty(options.MinStep)
    options.MinStep = 0;
  end
  if isempty(options.InitialStep)
    options.InitialStep = odeDefaultInitialStep(tspan, solver);
  end
  if isempty(options.Refine)
    options.Refine = odeDefaultRefine(solver);
  end
  options.AbsTol = odeExpandTolerance(options.AbsTol, y0);
end
%=============================================================================
function step = odeDefaultInitialStep(tspan, solver)
  interval = abs(tspan(length(tspan)) - tspan(1));
  switch solver
    case {'ode15i', 'idas'}
      step = interval / 40;
    otherwise
      step = interval / 100;
  end
end
%=============================================================================
function refine = odeDefaultRefine(solver)
  switch solver
    case 'ode45'
      refine = 4;
    case {'ode78', 'ode89'}
      refine = 8;
    otherwise
      refine = 1;
  end
end
%=============================================================================
function odeValidateRawOptions(options, y0, solver)
  odeValidatePositiveScalar(options.RelTol, 'RelTol', false);
  odeValidatePositiveScalar(options.MaxStep, 'MaxStep', false);
  odeValidatePositiveScalar(options.InitialStep, 'InitialStep', false);
  odeValidatePositiveScalar(options.MinStep, 'MinStep', false);
  odeValidatePositiveScalar(options.Refine, 'Refine', true);
  if ~isempty(options.Refine) && fix(options.Refine) ~= options.Refine
    error('Nelson:ode_solvers:refineIntegerScalar', 'Refine must be an integer scalar.');
  end
  if ~isempty(options.AbsTol) && (~isnumeric(options.AbsTol) || any(options.AbsTol(:) <= 0))
    error('Nelson:ode_solvers:positiveAbsoluteToleranceExpected', 'Positive absolute tolerance expected.');
  end
  if ~isempty(options.Events) && ~isa(options.Events, 'function_handle')
    error('Nelson:ode_solvers:eventFunctionHandleExpected', '%s function handle expected.', 'Event');
  end
  if ~isempty(options.OutputFcn) && ~isa(options.OutputFcn, 'function_handle')
    error('Nelson:ode_solvers:outputFunctionHandleExpected', '%s function handle expected.', 'Output');
  end
  odeValidateIndexOption(options.OutputSel, y0, 'OutputSel');
  odeValidateIndexOption(options.NonNegative, y0, 'NonNegative');
  odeValidateOnOff(options.Stats, 'Stats');
  odeValidateVectorized(options.Vectorized, solver);
  odeValidateOnOff(options.NormControl, 'NormControl');
  odeValidateOnOff(options.BDF, 'BDF');
  odeValidateOnOff(options.JConstant, 'JConstant');
  odeValidateChoice(options.LinearSolver, 'LinearSolver', ...
    {'auto', 'dense', 'spgmr', 'spfgmr', 'spbcgs', 'sptfqmr', 'pcg', 'klu'});
  odeValidateChoice(options.Preconditioner, 'Preconditioner', ...
    {'auto', 'none', 'jacobi', 'banded', 'ilu0'});
  odeValidateChoice(options.MassSingular, 'MassSingular', {'yes', 'no', 'maybe'});
  odeValidateChoice(options.MStateDependence, 'MStateDependence', {'none', 'weak', 'strong'});
  odeValidateMaxOrder(options.MaxOrder, solver);
  odeValidateMatrixOrFunction(options.Mass, length(y0(:)), 'Mass');
  odeValidateMatrixOrFunction(options.Jacobian, length(y0(:)), 'Jacobian');
  odeValidatePatternOption(options.JPattern, length(y0(:)), 'JPattern');
  odeValidatePatternOption(options.MvPattern, length(y0(:)), 'MvPattern');
end
%=============================================================================
function odeValidatePositiveScalar(value, name, allowZero)
  if isempty(value)
    return
  end
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value)
    error('Nelson:ode_solvers:mustBeFiniteScalar', '%s must be a finite scalar.', name);
  end
  if allowZero
    valid = value >= 0;
  else
    valid = value > 0;
  end
  if ~valid
    error('Nelson:validators:mustBePositive', '%s must be positive.', name);
  end
end
%=============================================================================
function odeValidateIndexOption(value, y0, name)
  if isempty(value)
    return
  end
  n = length(y0(:));
  if ~isnumeric(value) || any(value(:) < 1) || any(value(:) > n) || any(fix(value(:)) ~= value(:))
    error('Nelson:ode_solvers:invalidIndex', '%s indices are invalid.', name);
  end
end
%=============================================================================
function odeValidateOnOff(value, name)
  if isempty(value)
    return
  end
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value) || ~(strcmpi(value, 'on') || strcmpi(value, 'off'))
    error('Nelson:ode_solvers:invalidValue:valueOff', '%s must be ''on'' or ''off''.', name);
  end
end
%=============================================================================
function odeValidateVectorized(value, solver)
  if isempty(value)
    return
  end
  if iscell(value) && (strcmp(solver, 'ode15i') || strcmp(solver, 'idas'))
    if numel(value) ~= 2
      error('Nelson:ode_solvers:invalidValue:valueOff', '%s must be ''on'' or ''off''.', 'Vectorized');
    end
    odeValidateOnOff(value{1}, 'Vectorized');
    odeValidateOnOff(value{2}, 'Vectorized');
    return
  end
  odeValidateOnOff(value, 'Vectorized');
end
%=============================================================================
function odeValidateChoice(value, name, choices)
  if isempty(value)
    return
  end
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    error('Nelson:ode_solvers:invalidValue', '%s has an invalid value.', name);
  end
  valid = false;
  for k = 1:length(choices)
    valid = valid || strcmpi(value, choices{k});
  end
  if ~valid
    error('Nelson:ode_solvers:invalidValue', '%s has an invalid value.', name);
  end
end
%=============================================================================
function odeValidateMaxOrder(value, solver)
  if isempty(value)
    return
  end
  maximum = 5;
  if strcmp(solver, 'cvodesnonstiff')
    maximum = 12;
  end
  if ~isnumeric(value) || ~isscalar(value) || ~isfinite(value) ...
      || fix(value) ~= value || value < 1 || value > maximum
    error('Nelson:ode_solvers:maxorderIntegerScalarBetween', 'MaxOrder must be an integer scalar between %d and %d.', 1, maximum);
  end
end
%=============================================================================
function odeValidateMatrixOrFunction(value, n, name)
  if isempty(value) || isa(value, 'function_handle')
    return
  end
  if ~isnumeric(value)
    error('Nelson:ode_solvers:invalidType:valueNumericAFunctionHandle', '%s must be numeric or a function handle.', name);
  end
  if isscalar(value)
    return
  end
  if ~isequal(size(value), [n n])
    error('Nelson:ode_solvers:sizeMismatch', '%s matrix has incompatible size.', name);
  end
end
%=============================================================================
function odeValidatePatternOption(value, n, name)
  if isempty(value)
    return
  end
  if ~(isnumeric(value) || islogical(value))
    error('Nelson:ode_solvers:invalidType', '%s must be a numeric or logical matrix.', name);
  end
  if ~isequal(size(value), [n n])
    error('Nelson:ode_solvers:sizeMismatch', '%s matrix has incompatible size.', name);
  end
end
%=============================================================================
function tol = odeExpandTolerance(tol, y0)
  if isscalar(tol)
    tol = tol * ones(size(y0(:)));
  else
    tol = tol(:);
  end
  if length(tol) ~= length(y0(:))
    error('Nelson:ode_solvers:absoluteToleranceSizeNotMatchInitial', 'Absolute tolerance size does not match initial value.');
  end
end
%=============================================================================
