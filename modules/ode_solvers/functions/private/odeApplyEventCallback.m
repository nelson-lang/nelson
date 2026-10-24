%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [stop, eventY] = odeApplyEventCallback(problem, eventT, eventY, eventIndex)
  stop = false;
  if ~isfield(problem.options, 'EventDefinitionObject')
    return
  end
  eventDefinition = problem.options.EventDefinitionObject;
  if ~strcmpi(odeEventResponseAt(eventDefinition.Response, eventIndex), 'callback')
    return
  end
  callback = eventDefinition.CallbackFcn;
  if isempty(callback)
    error('Nelson:ode_solvers:callbackFunctionHandleExpected', '%s function handle expected.', 'Callback');
  end
  args = {eventT, eventY, eventIndex};
  if isfield(problem.options, 'Parameters') && ~isempty(problem.options.Parameters)
    args = odeAppendParameters(args, problem.options.Parameters);
  end
  nout = nargout(callback);
  if nout < 0
    nout = 2;
  end
  if nout <= 0
    feval(callback, args{:});
  elseif nout == 1
    stop = odeStopFlag(feval(callback, args{:}), 'Callback');
  else
    [stop, eventY] = feval(callback, args{:});
    stop = odeStopFlag(stop, 'Callback');
  end
end

function response = odeEventResponseAt(responses, index)
  if iscell(responses)
    if numel(responses) == 1
      response = responses{1};
    elseif index <= numel(responses)
      response = responses{index};
    else
      response = 'proceed';
    end
  else
    response = responses;
  end
end
%=============================================================================
