%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = odeRunDelaySolver(solver, odefun, tspan, y0, yp0, options, delay)
  odeValidateDelayProblem(solver, options, delay);
  valueDelays = odeDelayInitialValues(delay.ValueDelay, tspan(1), y0(:), options);
  slopeDelays = odeDelayInitialValues(delay.SlopeDelay, tspan(1), y0(:), options);
  allDelays = [valueDelays; slopeDelays];
  direction = sign(tspan(length(tspan)) - tspan(1));
  t0 = tspan(1);
  tfinal = tspan(length(tspan));
  history = delay.History;
  previousTimes = t0;
  previousValues = y0(:);
  previousSlopes = odeDelayInitialSlope(y0(:), yp0);
  aggregate = [];
  chunkStart = t0;
  chunkIndex = 0;
  while direction * (tfinal - chunkStart) > 0
    chunkIndex = chunkIndex + 1;
    chunkLength = odeDelayChunkLength(delay, chunkStart, previousValues(:, end), ...
      options, allDelays, tfinal, direction);
    chunkEnd = chunkStart + direction * chunkLength;
    [chunkSol, chunkEnd] = odeDelaySolveChunkWithRetry(solver, odefun, tspan, chunkStart, ...
      chunkEnd, direction, previousTimes, previousValues, previousSlopes, t0, history, ...
      delay, options);
    aggregate = odeDelayMergeSolution(aggregate, chunkSol, chunkIndex);
    previousTimes = aggregate.x;
    previousValues = aggregate.y;
    previousSlopes = aggregate.extdata.slopes;
    chunkStart = aggregate.x(length(aggregate.x));
    if direction * (chunkEnd - chunkStart) > eps * (1 + abs(chunkEnd))
      break
    end
  end
  aggregate = odeDelayFinalizeSolution(aggregate, solver, options, yp0);
  problem = odePrepareProblem(solver, odefun, tspan, y0, yp0, options);
  odeDisplayStats(problem, aggregate.stats);
  sol = aggregate;
end
%=============================================================================
function chunkLength = odeDelayChunkLength(delay, t, y, options, fallbackDelays, tfinal, direction)
  try
    values = [odeDelayCurrentValues(delay.ValueDelay, t, y, options); ...
      odeDelayCurrentValues(delay.SlopeDelay, t, y, options)];
  catch
    values = fallbackDelays;
  end
  if isempty(values)
    values = fallbackDelays;
  end
  chunkLength = min(values(:));
  remaining = abs(tfinal - t);
  chunkLength = min(chunkLength, remaining);
  if ~(isfinite(chunkLength) && chunkLength > 0)
    error('Nelson:ode_solvers:delayEntriesPositive', 'Delay entries must be positive.');
  end
  chunkLength = max(chunkLength, eps * (1 + abs(t)) * max(1, abs(direction)));
end
%=============================================================================
function [chunkSol, chunkEnd] = odeDelaySolveChunkWithRetry(solver, odefun, tspan, chunkStart, ...
    chunkEnd, direction, previousTimes, previousValues, previousSlopes, t0, history, delay, options)
  for attempt = 1:24
    try
      chunkSol = odeDelaySolveChunk(solver, odefun, tspan, chunkStart, chunkEnd, direction, ...
        previousTimes, previousValues, previousSlopes, t0, history, delay, options);
      return
    catch
      ex = lasterror();
      if ~odeDelayCanRetryChunk(ex, chunkStart, chunkEnd)
        error(ex.message);
      end
      chunkEnd = chunkStart + 0.5 * (chunkEnd - chunkStart);
    end
  end
  error('Nelson:ode_solvers:stateDependentDelayRequiresKnownPast', 'State-dependent delay requires a known past state.');
end
%=============================================================================
function tf = odeDelayCanRetryChunk(ex, chunkStart, chunkEnd)
  message = ex.message;
  tf = contains(message, _('State-dependent delay requires a known past state.')) ...
    && abs(chunkEnd - chunkStart) > eps * (1 + abs(chunkStart));
end
%=============================================================================
function chunkSol = odeDelaySolveChunk(solver, odefun, tspan, chunkStart, chunkEnd, direction, ...
    previousTimes, previousValues, previousSlopes, t0, history, delay, options)
  chunkTspan = odeDelayChunkTimeSpan(tspan, chunkStart, chunkEnd, direction);
  chunkOptions = options;
  chunkOptions.Stats = 'off';
  chunkOptions.Parameters = [];
  chunkOptions = odeDelayWrapParameterizedOptions(...
    chunkOptions, options, previousTimes, previousValues, previousSlopes, t0, history, delay);
  if odeDelayUsesImplicitResidual(solver)
    wrappedFcn = @(t, y, yp) odeDelayResidual(...
      odefun, t, y, yp, previousTimes, previousValues, previousSlopes, t0, history, ...
      delay.ValueDelay, delay.SlopeDelay, options);
  else
    wrappedFcn = @(t, y) odeDelayRhs(...
      odefun, t, y, previousTimes, previousValues, previousSlopes, t0, history, ...
      delay.ValueDelay, delay.SlopeDelay, options);
  end
  chunkSol = odeDelayRunChunkSolver(...
    solver, wrappedFcn, chunkTspan, previousValues(:, end), previousSlopes(:, end), chunkOptions);
end
%=============================================================================
function sol = odeDelayRunChunkSolver(solver, odefun, tspan, y0, yp0, options)
  if strcmp(solver, 'autoswitch')
    error('Nelson:ode_solvers:delayEquationsNotSupportAutoswitchChunks', 'Delay equations do not support autoswitch chunks.');
  end
  problem = odePrepareProblem(solver, odefun, tspan, y0, yp0, options);
  if odeIsSundialsSolver(problem.solver)
    if ~odeSundialsAvailable()
      error('Nelson:ode_solvers:sundialsBackendNotAvailable', 'SUNDIALS backend is not available.');
    end
    sol = __ode_sundials_run__(problem);
    sol = odeDelayApplySundialsOutputFcn(problem, sol);
    return
  end
  [x, y, slopes, xe, ye, ie, stats] = odeIntegrate(problem);
  sol = odeBuildSolution(problem, x, y, slopes, xe, ye, ie, stats);
end
%=============================================================================
function sol = odeDelayApplySundialsOutputFcn(problem, sol)
  if isempty(problem.options.OutputFcn)
    return
  end
  stop = odeCallOutput(problem, problem.tspan, problem.y0, 'init');
  stopIndex = [];
  if stop
    stopIndex = 1;
  else
    for k = 2:length(sol.x)
      if odeCallOutput(problem, sol.x(k), sol.y(:, k), '')
        stopIndex = k;
        break
      end
    end
  end
  odeCallOutput(problem, [], [], 'done');
  if ~isempty(stopIndex)
    sol = odeDelayTrimSundialsSolution(sol, stopIndex, problem.direction);
  end
end
%=============================================================================
function sol = odeDelayTrimSundialsSolution(sol, stopIndex, direction)
  sol.x = sol.x(1:stopIndex);
  sol.y = sol.y(:, 1:stopIndex);
  if isfield(sol, 'yp')
    sol.yp = sol.yp(:, 1:stopIndex);
  end
  if isfield(sol, 'extdata') && isfield(sol.extdata, 'slopes')
    sol.extdata.slopes = sol.extdata.slopes(:, 1:stopIndex);
  end
  if isfield(sol, 'xe')
    if direction > 0
      keep = sol.xe <= sol.x(end);
    else
      keep = sol.xe >= sol.x(end);
    end
    sol.xe = sol.xe(keep);
    sol.ye = sol.ye(:, keep);
    sol.ie = sol.ie(keep);
  end
end
%=============================================================================
function chunkOptions = odeDelayWrapParameterizedOptions(...
    chunkOptions, options, times, values, slopes, t0, history, delay)
  if isfield(options, 'EventDefinitionObject')
    eventDefinition = options.EventDefinitionObject;
    chunkOptions = rmfield(chunkOptions, 'EventDefinitionObject');
    chunkOptions.Events = @(t, y) odeDelayEventObject(eventDefinition, t, y, options);
  elseif ~isempty(options.Events)
    eventFcn = options.Events;
    chunkOptions.Events = @(t, y) odeDelayEventFunction(...
      eventFcn, t, y, options, times, values, slopes, t0, history, delay);
  end
  if isa(options.Mass, 'function_handle')
    massFcn = options.Mass;
    chunkOptions.Mass = @(t, y) odeDelayMassFunction(massFcn, t, y, options);
  end
end
%=============================================================================
function [value, isterminal, direction] = odeDelayEventObject(eventDefinition, t, y, options)
  args = {eventDefinition, t, y};
  args = odeAppendParameters(args, options.Parameters);
  [value, isterminal, direction] = odeEvaluateEventObject(args{:});
end
%=============================================================================
function [value, isterminal, direction] = odeDelayEventFunction(...
    eventFcn, t, y, options, times, values, slopes, t0, history, delay)
  [z, zp] = odeDelayEventDelayedValues(t, y, times, values, slopes, t0, history, delay, options);
  ninputs = nargin(eventFcn);
  if ninputs == -1 || ninputs >= 4
    [value, isterminal, direction] = feval(eventFcn, t, y, z, zp);
  elseif ninputs == 3
    [value, isterminal, direction] = feval(eventFcn, t, y, z);
  else
    localOptions = options;
    [value, isterminal, direction] = odeCallFcn(eventFcn, t, y, localOptions);
  end
end
%=============================================================================
function [z, zp] = odeDelayEventDelayedValues(t, y, times, values, slopes, t0, history, delay, options)
  valueDelays = odeDelayCurrentValues(delay.ValueDelay, t, y, options);
  z = zeros(length(y(:)), length(valueDelays));
  for k = 1:length(valueDelays)
    delayedT = odeDelayKnownTime(t, valueDelays(k), times);
    z(:, k) = odeDelayEvaluateState(delayedT, times, values, t0, history, options);
  end
  slopeDelays = odeDelayCurrentValues(delay.SlopeDelay, t, y, options);
  zp = zeros(length(y(:)), length(slopeDelays));
  for k = 1:length(slopeDelays)
    delayedT = odeDelayKnownTime(t, slopeDelays(k), times);
    zp(:, k) = odeDelayEvaluateSlope(delayedT, times, slopes, values(:, 1), t0, history, options);
  end
end
%=============================================================================
function mass = odeDelayMassFunction(massFcn, t, y, options)
  localOptions = options;
  mass = odeCallFcn(massFcn, t, y, localOptions);
end
%=============================================================================
function odeValidateDelayProblem(solver, options, delay)
  if strcmp(solver, 'cvodesnonstiff') || strcmp(solver, 'cvodesstiff')
    error('Nelson:ode_solvers:delayEquationsRequireOdeLinearlyImplicit', 'Delay equations require an ODE, linearly implicit, or fully implicit residual solver.');
  end
  if isempty(delay.ValueDelay) && isempty(delay.SlopeDelay)
    error('Nelson:ode_solvers:valuedelaySlopedelaySpecifiedDelayEquations', 'ValueDelay or SlopeDelay must be specified for delay equations.');
  end
  if ~isempty(delay.ValueDelay) && ~isa(delay.ValueDelay, 'function_handle') ...
      && (~isnumeric(delay.ValueDelay) || any(delay.ValueDelay(:) <= 0))
    error('Nelson:ode_solvers:valuedelayEntriesPositiveConstants', '%s entries must be positive constants.', 'ValueDelay');
  end
  if ~isempty(delay.SlopeDelay) && ~isa(delay.SlopeDelay, 'function_handle') ...
      && (~isnumeric(delay.SlopeDelay) || any(delay.SlopeDelay(:) <= 0))
    error('Nelson:ode_solvers:slopedelayEntriesPositiveConstants', '%s entries must be positive constants.', 'SlopeDelay');
  end
end
%=============================================================================
function yp = odeDelayInitialSlope(y0, yp0)
  if isempty(yp0)
    yp = zeros(size(y0));
  else
    yp = yp0(:);
  end
end
%=============================================================================
function tf = odeDelayUsesImplicitResidual(solver)
  tf = strcmp(solver, 'ode15i') || strcmp(solver, 'idas');
end
%=============================================================================
function values = odeDelayInitialValues(delayValue, t, y, options)
  if isempty(delayValue)
    values = [];
  elseif isa(delayValue, 'function_handle')
    values = odeDelayEvaluateDelayFunction(delayValue, t, y, options);
  else
    values = delayValue(:);
  end
  odeDelayCheckPositive(values);
end
%=============================================================================
function tspanChunk = odeDelayChunkTimeSpan(tspan, t0, tfinal, direction)
  inside = tspan(direction * (tspan - t0) > 0 & direction * (tfinal - tspan) > 0);
  tspanChunk = [t0 inside tfinal];
end
%=============================================================================
function dydt = odeDelayRhs(...
    odefun, t, y, times, values, slopes, t0, history, valueDelays, slopeDelays, options)
  valueDelays = odeDelayCurrentValues(valueDelays, t, y, options);
  slopeDelays = odeDelayCurrentValues(slopeDelays, t, y, options);
  z = zeros(length(y(:)), length(valueDelays));
  for k = 1:length(valueDelays)
    delayedT = odeDelayKnownTime(t, valueDelays(k), times);
    z(:, k) = odeDelayEvaluateState(delayedT, times, values, t0, history, options);
  end
  if isempty(slopeDelays)
    args = {t, y, z};
  else
    zp = zeros(length(y(:)), length(slopeDelays));
    for k = 1:length(slopeDelays)
      delayedT = odeDelayKnownTime(t, slopeDelays(k), times);
      zp(:, k) = odeDelayEvaluateSlope(...
        delayedT, times, slopes, values(:, 1), t0, history, options);
    end
    args = {t, y, z, zp};
  end
  args = odeAppendParameters(args, options.Parameters);
  dydt = feval(odefun, args{:});
  dydt = dydt(:);
end
%=============================================================================
function values = odeDelayCurrentValues(delayValue, t, y, options)
  if isempty(delayValue)
    values = [];
  elseif isa(delayValue, 'function_handle')
    values = odeDelayEvaluateDelayFunction(delayValue, t, y, options);
  else
    values = delayValue(:);
  end
  odeDelayCheckPositive(values);
end
%=============================================================================
function values = odeDelayEvaluateDelayFunction(fcn, t, y, options)
  args = {t, y};
  if isfield(options, 'Parameters') && ~isempty(options.Parameters)
    args = odeAppendParameters(args, options.Parameters);
  end
  values = feval(fcn, args{:});
  if ~isnumeric(values) || any(~isfinite(values(:)))
    error('Nelson:ode_solvers:delayFunctionReturnFiniteNumericValues', 'Delay function must return finite numeric values.');
  end
  values = values(:);
end
%=============================================================================
function residual = odeDelayResidual(...
    odefun, t, y, yp, times, values, slopes, t0, history, valueDelays, slopeDelays, options)
  valueDelays = odeDelayCurrentValues(valueDelays, t, y, options);
  slopeDelays = odeDelayCurrentValues(slopeDelays, t, y, options);
  z = zeros(length(y(:)), length(valueDelays));
  for k = 1:length(valueDelays)
    delayedT = odeDelayKnownTime(t, valueDelays(k), times);
    z(:, k) = odeDelayEvaluateState(delayedT, times, values, t0, history, options);
  end
  if isempty(slopeDelays)
    args = {t, y, yp, z};
  else
    zp = zeros(length(y(:)), length(slopeDelays));
    for k = 1:length(slopeDelays)
      delayedT = odeDelayKnownTime(t, slopeDelays(k), times);
      zp(:, k) = odeDelayEvaluateSlope(...
        delayedT, times, slopes, values(:, 1), t0, history, options);
    end
    args = {t, y, yp, z, zp};
  end
  args = odeAppendParameters(args, options.Parameters);
  residual = feval(odefun, args{:});
  residual = residual(:);
end
%=============================================================================
function odeDelayCheckPositive(values)
  if any(values(:) <= 0)
    error('Nelson:ode_solvers:delayEntriesPositive', 'Delay entries must be positive.');
  end
end
%=============================================================================
function delayedT = odeDelayKnownTime(t, delayValue, times)
  delayedT = t - delayValue;
  tolerance = max(1e-8, sqrt(eps) * (1 + abs(t)));
  if delayedT > times(length(times)) + tolerance
    error('Nelson:ode_solvers:stateDependentDelayRequiresKnownPast', 'State-dependent delay requires a known past state.');
  end
  delayedT = min(delayedT, times(length(times)));
end
%=============================================================================
function y = odeDelayEvaluateState(t, times, values, t0, history, options)
  if t <= t0
    y = odeDelayEvaluateHistory(t, values(:, 1), history, options);
    return
  end
  if length(times) == 1
    y = values(:, 1);
    return
  end
  y = interp1(times(:), values.', t, 'linear').';
end
%=============================================================================
function yp = odeDelayEvaluateSlope(t, times, slopes, y0, t0, history, options)
  if t <= t0
    yp = odeDelayEvaluateHistorySlope(t, y0, history, options);
    return
  end
  if length(times) == 1 || isempty(slopes)
    yp = zeros(size(y0));
    return
  end
  yp = interp1(times(:), slopes.', t, 'linear').';
end
%=============================================================================
function yp = odeDelayEvaluateHistorySlope(t, y0, history, options)
  if isempty(history) || isnumeric(history)
    yp = zeros(size(y0));
    return
  end
  if isstruct(history)
    [~, yp] = deval(history, t);
    yp = yp(:);
    return
  end
  step = sqrt(eps) * max(1, abs(t));
  yplus = odeDelayEvaluateHistory(t + step, y0, history, options);
  yminus = odeDelayEvaluateHistory(t - step, y0, history, options);
  yp = (yplus - yminus) / (2 * step);
end
%=============================================================================
function y = odeDelayEvaluateHistory(t, y0, history, options)
  if isempty(history)
    y = y0;
    return
  end
  if isa(history, 'function_handle')
    args = odeAppendParameters({t}, options.Parameters);
    y = feval(history, args{:});
  elseif isstruct(history)
    y = deval(history, t);
  elseif isscalar(history)
    y = history * ones(size(y0));
  else
    y = history(:);
  end
  if length(y(:)) ~= length(y0(:))
    error('Nelson:ode_solvers:historySizeNotMatchInitialValue', 'History size does not match initial value.');
  end
  y = y(:);
end
%=============================================================================
function aggregate = odeDelayMergeSolution(aggregate, chunkSol, chunkIndex)
  if isempty(aggregate)
    aggregate = chunkSol;
    aggregate.stats.nchunks = chunkIndex;
    return
  end
  first = 2;
  aggregate.x = [aggregate.x chunkSol.x(first:end)];
  aggregate.y = [aggregate.y chunkSol.y(:, first:end)];
  aggregate.extdata.slopes = [aggregate.extdata.slopes chunkSol.extdata.slopes(:, first:end)];
  if isfield(chunkSol, 'xe')
    if isfield(aggregate, 'xe')
      aggregate.xe = [aggregate.xe chunkSol.xe];
      aggregate.ye = [aggregate.ye chunkSol.ye];
      aggregate.ie = [aggregate.ie chunkSol.ie];
    else
      aggregate.xe = chunkSol.xe;
      aggregate.ye = chunkSol.ye;
      aggregate.ie = chunkSol.ie;
    end
  end
  aggregate.stats = odeDelayMergeStats(aggregate.stats, chunkSol.stats);
  aggregate.stats.nchunks = chunkIndex;
end
%=============================================================================
function stats = odeDelayMergeStats(stats, chunkStats)
  names = {'nsteps'; 'nfailed'; 'nfevals'};
  for k = 1:length(names)
    name = names{k};
    if isfield(stats, name) && isfield(chunkStats, name)
      stats.(name) = stats.(name) + chunkStats.(name);
    end
  end
end
%=============================================================================
function sol = odeDelayFinalizeSolution(sol, solver, options, yp0)
  sol.solver = solver;
  sol.extdata.options = options;
  sol.extdata.yp0 = yp0;
  if ~isfield(sol.stats, 'nchunks')
    sol.stats.nchunks = 1;
  end
end
%=============================================================================
