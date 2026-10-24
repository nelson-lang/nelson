%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, y, slopes, xe, ye, ie, stats] = odeIntegrate(problem)
  t = problem.t0;
  state = problem.y0;
  h = problem.direction * min(problem.options.InitialStep, problem.options.MaxStep);
  currentSlope = odeRhs(problem, t, state);
  [x, y, slopes, nStored] = odeInitStoredSteps(problem, t, state, currentSlope);
  requestedTimes = problem.tspan(2:length(problem.tspan));
  requestedTimes = requestedTimes(:).';
  nextRequested = 1;
  xe = [];
  ye = zeros(length(state), 0);
  ie = [];
  stats = odeInitialStats();
  solverState = odeInitialSolverState(problem, t, state, currentSlope);
  autoSwitch = odeInitialAutoSwitch(problem);
  eventState = odeInitialEventState(problem, t, state);
  if odeCallOutput(problem, problem.tspan, state, 'init')
    stats.tfinal = t;
    [x, y, slopes] = odeTrimStoredSteps(x, y, slopes, nStored);
    return
  end
  guard = 0;
  while problem.direction * (problem.tfinal - t) > 0
    guard = guard + 1;
    if guard > 100000
      warning('Nelson:ode:integrationFailure', odeIntegrationFailureMessage(t));
      break
    end
    if problem.direction * (problem.tfinal - t) <= eps * (1 + abs(problem.tfinal))
      t = problem.tfinal;
      x(nStored) = t;
      break
    end
    h = odeClampStep(problem, t, h);
    if abs(h) <= eps * (1 + abs(t))
      warning('Nelson:ode:integrationFailure', odeIntegrationFailureMessage(t));
      break
    end
    [accepted, nextState, err, evals] = odeTryStep(problem, t, state, h, solverState);
    stats.nfevals = stats.nfevals + evals;
    if accepted
      stats.nsteps = stats.nsteps + 1;
      oldT = t;
      oldState = state;
      oldSlope = currentSlope;
      candidateT = t + h;
      candidateState = odeApplyNonNegative(problem, nextState);
      [eventState, stop, ev] = odeCheckEvents(problem, eventState, oldT, oldState, candidateT, candidateState);
      if stop && ~isempty(ev.t)
        t = ev.t(length(ev.t));
        state = ev.y(:, size(ev.y, 2));
      else
        t = candidateT;
        state = candidateState;
      end
      newSlope = odeRhs(problem, t, state);
      if isempty(problem.options.OutputFcn)
        outputStop = false;
        outputT = t;
        outputY = state;
      else
        [outputStop, outputT, outputY] = odeCallStepOutput(problem, oldT, oldState, oldSlope, t, state, newSlope, stop);
      end
      if outputStop
        t = outputT;
        state = odeApplyNonNegative(problem, outputY);
        newSlope = odeRhs(problem, t, state);
        ev = odeFilterEvents(ev, t, problem.direction);
      end
      [x, y, slopes, nStored, nextRequested] = odeAppendStoredStep(...
        problem, x, y, slopes, nStored, requestedTimes, nextRequested, ...
        oldT, oldState, oldSlope, t, state, newSlope, stop || outputStop);
      solverState = odeAcceptStep(problem, solverState, oldT, oldState, oldSlope, h, t, state, newSlope);
      currentSlope = newSlope;
      solverState.consecutiveRejections = 0;
      [problem, solverState, autoSwitch, h] = odeAutoSwitchAfterStep(...
        problem, solverState, autoSwitch, h, t, state, newSlope, err, stats);
      [xe, ye, ie] = odeAppendEvent(xe, ye, ie, ev);
      if outputStop
        break
      end
      if stop
        break
      end
    else
      stats.nfailed = stats.nfailed + 1;
      [problem, solverState, autoSwitch, h] = odeAutoSwitchAfterFailure(...
        problem, solverState, autoSwitch, h, t, state, currentSlope, stats);
      solverState = odeReduceOrderOnFailure(problem, solverState);
    end
    h = odeNextStep(problem, h, err, accepted, stats.nsteps, solverState);
  end
  stats.tfinal = t;
  stats = odeFinalizeAutoSwitchStats(stats, autoSwitch);
  odeCallOutput(problem, [], [], 'done');
  [x, y, slopes] = odeTrimStoredSteps(x, y, slopes, nStored);
end
%=============================================================================
function [x, y, slopes, nStored] = odeInitStoredSteps(problem, t, state, slope)
  % Storage is preallocated and grown geometrically; nStored tracks the
  % filled prefix so appending stays amortized O(1) per stored column.
  if odeStoreRequestedOutputOnly(problem)
    capacity = length(problem.tspan) + 1;
  else
    capacity = 64;
  end
  n = length(state);
  x = zeros(1, capacity);
  y = zeros(n, capacity);
  slopes = zeros(n, capacity);
  nStored = 1;
  x(1) = t;
  y(:, 1) = state;
  slopes(:, 1) = slope;
end
%=============================================================================
function [x, y, slopes] = odeEnsureStoredCapacity(x, y, slopes, needed)
  capacity = length(x);
  if needed <= capacity
    return
  end
  newCapacity = max(needed, 2 * capacity);
  x(1, newCapacity) = 0;
  y(size(y, 1), newCapacity) = 0;
  slopes(size(slopes, 1), newCapacity) = 0;
end
%=============================================================================
function [x, y, slopes] = odeTrimStoredSteps(x, y, slopes, nStored)
  x = x(1:nStored);
  y = y(:, 1:nStored);
  slopes = slopes(:, 1:nStored);
end
%=============================================================================
function [x, y, slopes, nStored, nextRequested] = odeAppendStoredStep(...
    problem, x, y, slopes, nStored, requestedTimes, nextRequested, ...
    oldT, oldState, oldSlope, t, state, newSlope, terminalStep)
  if ~odeStoreRequestedOutputOnly(problem)
    [x, y, slopes] = odeEnsureStoredCapacity(x, y, slopes, nStored + 1);
    nStored = nStored + 1;
    x(nStored) = t;
    y(:, nStored) = state;
    slopes(:, nStored) = newSlope;
    return
  end
  [outputTimes, nextRequested] = odeConsumeRequestedOutputTimes(...
    problem.direction, requestedTimes, nextRequested, oldT, t);
  if terminalStep
    outputTimes = odeAppendUniqueOutputTime(outputTimes, t);
  end
  if isempty(outputTimes)
    return
  end
  [x, y, slopes] = odeEnsureStoredCapacity(x, y, slopes, nStored + length(outputTimes));
  for k = 1:length(outputTimes)
    outputT = outputTimes(k);
    if abs(outputT - t) <= eps * (1 + abs(t))
      outputY = state;
      outputSlope = newSlope;
    else
      outputY = odeDevalInterpolate([oldT t], [oldState state], outputT, [oldSlope newSlope]);
      outputSlope = odeLinearInterpolate([oldT t], [oldSlope newSlope], outputT);
    end
    nStored = nStored + 1;
    x(nStored) = outputT;
    y(:, nStored) = outputY;
    slopes(:, nStored) = outputSlope;
  end
end
%=============================================================================
function tf = odeStoreRequestedOutputOnly(problem)
  tf = isfield(problem, 'storeRequestedOutputOnly') && problem.storeRequestedOutputOnly;
end
%=============================================================================
function autoSwitch = odeInitialAutoSwitch(problem)
  if isfield(problem, 'autoSwitch') && problem.autoSwitch.enabled
    autoSwitch = problem.autoSwitch;
  else
    autoSwitch = struct('enabled', false);
  end
  autoSwitch.consecutiveFailures = 0;
  autoSwitch.stiffnessIndicator = 0;
  autoSwitch.releaseIndicator = 0;
  autoSwitch.stiffnessCriterion = 'errorPressure';
end
%=============================================================================
function [problem, solverState, autoSwitch, h] = odeAutoSwitchAfterStep(...
    problem, solverState, autoSwitch, h, t, y, slope, err, stats)
  if ~autoSwitch.enabled
    return
  end
  autoSwitch.consecutiveFailures = 0;
  if strcmp(problem.solver, autoSwitch.stiffSolver)
    [problem, solverState, autoSwitch, h] = odeAutoSwitchMaybeReturnNonstiff(...
      problem, solverState, autoSwitch, h, t, y, slope, err);
    return
  end
  [needsStiff, indicator] = odeAutoSwitchNeedsStiff(problem, h, slope, err, stats);
  autoSwitch.stiffnessIndicator = indicator;
  if ~needsStiff
    return
  end
  [problem, solverState, autoSwitch, h] = odeAutoSwitchToStiff(...
    problem, solverState, autoSwitch, h, t, y, slope, 'stiffnessHeuristic');
end
%=============================================================================
function [problem, solverState, autoSwitch, h] = odeAutoSwitchAfterFailure(...
    problem, solverState, autoSwitch, h, t, y, slope, stats)
  if ~autoSwitch.enabled || strcmp(problem.solver, autoSwitch.stiffSolver)
    return
  end
  autoSwitch.consecutiveFailures = autoSwitch.consecutiveFailures + 1;
  if autoSwitch.consecutiveFailures < 3 && stats.nfailed < 4
    return
  end
  [problem, solverState, autoSwitch, h] = odeAutoSwitchToStiff(...
    problem, solverState, autoSwitch, h, t, y, slope, 'nonstiffFailure');
end
%=============================================================================
function [problem, solverState, autoSwitch, h] = odeAutoSwitchToStiff(...
    problem, solverState, autoSwitch, h, t, y, slope, reason)
  problem.solver = autoSwitch.stiffSolver;
  problem.errorExponent = -1 / 3;
  problem.options = odeAutoSwitchStiffOptions(problem.options);
  solverState = odeInitialSolverState(problem, t, y, slope);
  autoSwitch.selectedSolver = autoSwitch.stiffSolver;
  autoSwitch.switched = true;
  autoSwitch.reason = reason;
  autoSwitch.lastReason = reason;
  autoSwitch.switchTime = t;
  autoSwitch.switchCount = autoSwitch.switchCount + 1;
  autoSwitch.nonstiffCandidateSteps = 0;
  h = problem.direction * min(abs(h), problem.options.MaxStep);
end
%=============================================================================
function [problem, solverState, autoSwitch, h] = odeAutoSwitchMaybeReturnNonstiff(...
    problem, solverState, autoSwitch, h, t, y, slope, err)
  [canReturn, indicator] = odeAutoSwitchCanReturnNonstiff(h, slope, err);
  autoSwitch.releaseIndicator = indicator;
  if canReturn
    autoSwitch.nonstiffCandidateSteps = autoSwitch.nonstiffCandidateSteps + 1;
  else
    autoSwitch.nonstiffCandidateSteps = 0;
  end
  % Three consecutive comfortable stiff steps signal that stiffness has
  % passed; the variable-order stiff kernel takes large steps, so a longer
  % streak may not fit in the remaining integration interval. The wide
  % hysteresis between the release indicator (pressure/0.5) and the stiff
  % re-entry heuristic (pressure/25) prevents switch thrashing.
  if autoSwitch.nonstiffCandidateSteps < 3
    return
  end
  problem.solver = autoSwitch.initialSolver;
  problem.errorExponent = -1 / 5;
  solverState = odeInitialSolverState(problem, t, y, slope);
  autoSwitch.selectedSolver = autoSwitch.initialSolver;
  autoSwitch.lastReason = 'stiffnessReleased';
  autoSwitch.switchTime = t;
  autoSwitch.switchCount = autoSwitch.switchCount + 1;
  autoSwitch.nonstiffCandidateSteps = 0;
  h = problem.direction * min(abs(h), problem.options.MaxStep);
end
%=============================================================================
function [tf, indicator] = odeAutoSwitchCanReturnNonstiff(h, slope, err)
  pressure = abs(h) * max(1, norm(slope(:), inf));
  % err is the true scaled error of an accepted step (<= 1); comfortable
  % acceptance means err well below 1, hence the 0.5 threshold.
  indicator = max(err / 0.5, pressure / 0.5);
  tf = indicator < 1;
end
%=============================================================================
function [tf, indicator] = odeAutoSwitchNeedsStiff(problem, h, slope, err, stats)
  if ~strcmp(problem.solver, 'ode45')
    tf = false;
    indicator = 0;
    return
  end
  scale = max(1, norm(slope(:), inf));
  pressure = abs(h) * scale;
  tooManyFailures = stats.nfailed >= 4;
  indicator = max(err / 0.8, pressure / 25);
  tf = indicator > 1 || tooManyFailures;
end
%=============================================================================
function options = odeAutoSwitchStiffOptions(options)
  if isfield(options, 'MaxOrder') && ~isempty(options.MaxOrder) && options.MaxOrder > 5
    options.MaxOrder = 5;
  end
end
%=============================================================================
function stats = odeFinalizeAutoSwitchStats(stats, autoSwitch)
  if ~autoSwitch.enabled
    return
  end
  stats.autoSwitchInitialSolver = autoSwitch.initialSolver;
  stats.autoSwitchSelectedSolver = autoSwitch.selectedSolver;
  stats.autoSwitchSwitched = autoSwitch.switched;
  stats.autoSwitchReason = autoSwitch.reason;
  if isfield(autoSwitch, 'lastReason')
    stats.autoSwitchLastReason = autoSwitch.lastReason;
  else
    stats.autoSwitchLastReason = autoSwitch.reason;
  end
  stats.autoSwitchMode = autoSwitch.mode;
  stats.autoSwitchSwitchTime = autoSwitch.switchTime;
  stats.autoSwitchSwitchCount = autoSwitch.switchCount;
  stats.autoSwitchStiffnessIndicator = autoSwitch.stiffnessIndicator;
  stats.autoSwitchReleaseIndicator = autoSwitch.releaseIndicator;
  stats.autoSwitchStiffnessCriterion = autoSwitch.stiffnessCriterion;
end
%=============================================================================
function [stop, stopT, stopY] = odeCallStepOutput(problem, t0, y0, yp0, t1, y1, yp1, terminalStep)
  stop = false;
  stopT = t1;
  stopY = y1;
  outputTimes = odeStepOutputTimes(problem, t0, t1, terminalStep);
  for k = 1:length(outputTimes)
    [outputY, ~] = odeLinearInterpolate([t0 t1], [y0 y1], outputTimes(k), [yp0 yp1]);
    if odeCallOutput(problem, outputTimes(k), outputY, '')
      stop = true;
      stopT = outputTimes(k);
      stopY = outputY;
      return
    end
  end
end
%=============================================================================
function outputTimes = odeStepOutputTimes(problem, t0, t1, terminalStep)
  if length(problem.tspan) > 2
    outputTimes = odeRequestedOutputTimes(problem, t0, t1);
  else
    outputTimes = odeRefinedOutputTimes(problem, t0, t1);
  end
  if terminalStep
    outputTimes = odeAppendUniqueOutputTime(outputTimes, t1);
  end
end
%=============================================================================
function [outputTimes, nextRequested] = odeConsumeRequestedOutputTimes(...
    direction, requested, nextRequested, t0, t1)
  % Cursor-based variant of odeRequestedOutputTimes: steps and requested
  % output times both advance monotonically, so each requested point is
  % visited once over the whole integration (amortized O(1) per step)
  % instead of rescanning the full tspan on every accepted step.
  total = length(requested);
  startTol = eps * (1 + abs(t0));
  endTol = eps * (1 + abs(t1));
  first = nextRequested;
  while first <= total && direction * (requested(first) - t0) <= startTol
    first = first + 1;
  end
  last = first - 1;
  while last < total && direction * (t1 - requested(last + 1)) >= -endTol
    last = last + 1;
  end
  outputTimes = requested(first:last);
  nextRequested = last + 1;
end
%=============================================================================
function outputTimes = odeRequestedOutputTimes(problem, t0, t1)
  direction = problem.direction;
  requested = problem.tspan(2:length(problem.tspan));
  afterStart = direction * (requested - t0) > eps * (1 + abs(t0));
  beforeEnd = direction * (t1 - requested) >= -eps * (1 + abs(t1));
  outputTimes = requested(afterStart & beforeEnd);
  outputTimes = outputTimes(:).';
end
%=============================================================================
function outputTimes = odeRefinedOutputTimes(problem, t0, t1)
  refine = problem.options.Refine;
  if isempty(refine) || refine < 1
    refine = 1;
  end
  outputTimes = zeros(1, refine);
  for k = 1:refine
    outputTimes(k) = t0 + (k / refine) * (t1 - t0);
  end
end
%=============================================================================
function outputTimes = odeAppendUniqueOutputTime(outputTimes, value)
  if isempty(outputTimes) || all(abs(outputTimes - value) > eps * (1 + abs(value)))
    outputTimes(end + 1) = value;
  end
end
%=============================================================================
function ev = odeFilterEvents(ev, t, direction)
  if isempty(ev.t)
    return
  end
  keep = direction * (t - ev.t) >= -eps * (1 + abs(t));
  ev.t = ev.t(keep);
  ev.y = ev.y(:, keep);
  ev.i = ev.i(keep);
end
%=============================================================================
function message = odeIntegrationFailureMessage(t)
  minStep = eps * (1 + abs(t));
  message = sprintf(_(['Failure at t=%g. Unable to meet integration tolerances ', ...
    'without reducing the step size below the smallest value allowed (%g) at time t.']), ...
    t, minStep);
end
%=============================================================================
