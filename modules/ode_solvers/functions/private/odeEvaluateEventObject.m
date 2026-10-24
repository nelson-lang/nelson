%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [value, isterminal, direction] = odeEvaluateEventObject(eventDefinition, t, y, varargin)
  fcn = eventDefinition.EventFcn;
  if isempty(fcn)
    error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
  end
  args = odeEventArguments(fcn, t, y, varargin{:});
  if odeEventFunctionHasLegacyOutputs(fcn)
    [value, isterminal, direction] = feval(fcn, args{:});
  else
    value = feval(fcn, args{:});
    [isterminal, direction] = odeEventFlags(eventDefinition, value);
  end
end
%=============================================================================
function tf = odeEventFunctionHasLegacyOutputs(fcn)
  nout = nargout(fcn);
  tf = nout >= 3;
end
%=============================================================================
function args = odeEventArguments(fcn, t, y, varargin)
  args = [{t, y}, varargin];
  nin = nargin(fcn);
  if nin >= 0 && nin < length(args)
    args = args(1:nin);
  end
end
%=============================================================================
function [isterminal, direction] = odeEventFlags(eventDefinition, value)
  n = numel(value);
  isterminal = false(n, 1);
  direction = zeros(n, 1);
  for k = 1:n
    response = odeEventChoiceAt(eventDefinition.Response, k, n, 'Response');
    if strcmpi(response, 'stop')
      isterminal(k) = true;
    end
    directionChoice = odeEventChoiceAt(eventDefinition.Direction, k, n, 'Direction');
    if strcmpi(directionChoice, 'increasing')
      direction(k) = 1;
    elseif strcmpi(directionChoice, 'decreasing')
      direction(k) = -1;
    end
  end
end
%=============================================================================
function choice = odeEventChoiceAt(choices, index, count, name)
  if iscell(choices)
    if numel(choices) == 1
      choice = choices{1};
    elseif numel(choices) == count
      choice = choices{index};
    else
      error('Nelson:ode_solvers:sizeMismatch:valueSizeDoesNotMatchEvent', '%s size does not match event value size.', name);
    end
  else
    choice = choices;
  end
end
%=============================================================================
