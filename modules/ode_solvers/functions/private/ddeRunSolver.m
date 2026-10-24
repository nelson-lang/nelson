%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = ddeRunSolver(name, ddefun, delays, slopeDelays, history, tspan, options)
  ddeValidateInputs(ddefun, tspan);
  ddeOptions = ddeNormalizeOptions(options);
  [y0, yp0] = ddeInitialState(history, tspan(1), ddeOptions);
  tspan = ddeApplyJumps(tspan, ddeOptions.Jumps);
  options = ddeOptionsToOdeOptions(ddeOptions);
  [delays, slopeDelays] = ddeNormalizeDelays(name, delays, slopeDelays);
  delay = odeDelay('History', ddeStateHistory(history), ...
    'ValueDelay', delays, 'SlopeDelay', slopeDelays);
  sol = odeRunDelaySolver('ode45', ddefun, tspan, y0, yp0, options, delay);
  sol.solver = name;
  if isfield(sol, 'extdata') && isfield(sol.extdata, 'slopes')
    sol.yp = sol.extdata.slopes;
  else
    [~, sol.yp] = deval(sol, sol.x);
  end
end
%=============================================================================
function history = ddeStateHistory(history)
  if iscell(history)
    history = history{1};
  end
end
%=============================================================================
function options = ddeOptionsToOdeOptions(ddeOptions)
  options = odeDefaultOptions();
  names = {'AbsTol', 'Events', 'InitialStep', 'MaxStep', 'NormControl', ...
    'OutputFcn', 'OutputSel', 'Refine', 'RelTol', 'Stats'};
  for k = 1:length(names)
    options.(names{k}) = ddeOptions.(names{k});
  end
  options.Parameters = [];
end
%=============================================================================
function [delays, slopeDelays] = ddeNormalizeDelays(name, delays, slopeDelays)
  if strcmp(name, 'dde23')
    return
  end
  delays = ddeWrapDelayTimes(delays);
  slopeDelays = ddeWrapDelayTimes(slopeDelays);
end
%=============================================================================
function delayValue = ddeWrapDelayTimes(delayValue)
  if isempty(delayValue) || ~isa(delayValue, 'function_handle')
    return
  end
  delayFcn = delayValue;
  delayValue = @(t, y) ddeDelayTimesToLags(delayFcn, t, y);
end
%=============================================================================
function lags = ddeDelayTimesToLags(delayFcn, t, y)
  delayedTimes = feval(delayFcn, t, y);
  lags = t - min(delayedTimes(:), t);
  lags = max(lags, sqrt(eps) * (1 + abs(t)));
end
%=============================================================================
function ddeValidateInputs(ddefun, tspan)
  if ~isa(ddefun, 'function_handle')
    error('Nelson:ode_solvers:ddeFunctionHandleExpected', '%s function handle expected.', 'DDE');
  end
  if ~isnumeric(tspan) || numel(tspan) < 2 || any(~isfinite(tspan(:)))
    error('Nelson:ode_solvers:timeSpanContainLeastTwoFinite', 'Time span must contain at least two finite numeric values.');
  end
  if tspan(1) == tspan(numel(tspan))
    error('Nelson:ode_solvers:timeSpanEndpointsDistinct', 'Time span endpoints must be distinct.');
  end
end
%=============================================================================
function options = ddeNormalizeOptions(options)
  if isempty(options)
    options = ddeset();
  elseif ~isstruct(options)
    error('Nelson:ode_solvers:ddeOptionsStructureExpected', '%s options structure expected.', 'DDE');
  end
  options = ddeMergeOptions(ddeDefaultOptions(), options);
  if ~isfield(options, 'Parameters')
    options.Parameters = [];
  end
end
%=============================================================================
function tspan = ddeApplyJumps(tspan, jumps)
  tspan = tspan(:).';
  if isempty(jumps)
    return
  end
  if ~isnumeric(jumps) || any(~isfinite(jumps(:)))
    error('Nelson:ode_solvers:jumpsFiniteNumericValues', 'Jumps must be finite numeric values.');
  end
  direction = sign(tspan(length(tspan)) - tspan(1));
  inside = jumps(direction * (jumps - tspan(1)) > 0 ...
    & direction * (tspan(length(tspan)) - jumps) > 0);
  tspan = [tspan inside(:).'];
  if direction < 0
    tspan = sort(tspan, 'descend');
  else
    tspan = sort(tspan);
  end
  tspan = unique(tspan, 'stable');
end
%=============================================================================
function [y0, yp0] = ddeInitialState(history, t0, options)
  yp0 = [];
  if ~isempty(options.InitialY)
    y0 = options.InitialY;
  elseif iscell(history)
    if length(history) ~= 2
      error('Nelson:ode_solvers:historyCellArrayContainInitialValues', 'History cell array must contain initial values and slopes.');
    end
    y0 = history{1};
    yp0 = history{2};
  elseif isa(history, 'function_handle')
    y0 = feval(history, t0);
  elseif isstruct(history)
    [y0, yp0] = deval(history, t0);
  elseif isnumeric(history)
    y0 = history;
  else
    error('Nelson:ode_solvers:historyNumericFunctionHandleCellArray', 'History must be numeric, a function handle, a cell array, or a solution structure.');
  end
  if ~isnumeric(y0) || isempty(y0) || any(~isfinite(y0(:)))
    error('Nelson:ode_solvers:historyEvaluateFiniteNumericValues', 'History must evaluate to finite numeric values.');
  end
  y0 = y0(:);
  if ~isempty(yp0)
    yp0 = yp0(:);
  end
end
%=============================================================================
