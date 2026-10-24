%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [value, isterminal, direction] = odeCallEvents(problem, t, y)
  if isfield(problem.options, 'EventDefinitionObject')
    [value, isterminal, direction] = odeCallEventObject(problem, t, y);
  elseif strcmp(problem.solver, 'ode15i')
    yp = odeRhs(problem, t, y);
    [value, isterminal, direction] = odeCallFcn(problem.options.Events, t, y, problem.options, yp);
  else
    [value, isterminal, direction] = odeCallFcn(problem.options.Events, t, y, problem.options);
  end
  [value, isterminal, direction] = odeValidateEventOutputs(value, isterminal, direction);
end
%=============================================================================
function [value, isterminal, direction] = odeValidateEventOutputs(value, isterminal, direction)
  if ~isnumeric(value) || isempty(value) || any(~isfinite(value(:)))
    error('Nelson:ode_solvers:eventValuesFiniteNumericValues', 'Event values must be finite numeric values.');
  end
  if ~(isnumeric(isterminal) || islogical(isterminal)) || isempty(isterminal) ...
      || any(~isfinite(double(isterminal(:))))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Event terminal flags');
  end
  if ~isnumeric(direction) || isempty(direction) || any(~isfinite(direction(:)))
    error('Nelson:ode_solvers:eventDirectionsFiniteNumericValues', 'Event directions must be finite numeric values.');
  end
  n = numel(value);
  if numel(isterminal) ~= n || numel(direction) ~= n
    error('Nelson:ode_solvers:eventOutputsSameNumberElements', 'Event outputs must have the same number of elements.');
  end
  value = value(:);
  isterminal = isterminal(:);
  direction = direction(:);
end
%=============================================================================
function [value, isterminal, direction] = odeCallEventObject(problem, t, y)
  args = {problem.options.EventDefinitionObject, t, y};
  if strcmp(problem.solver, 'ode15i')
    yp = odeRhs(problem, t, y);
    args = [args, {yp}];
  end
  if isfield(problem.options, 'Parameters') && ~isempty(problem.options.Parameters)
    args = odeAppendParameters(args, problem.options.Parameters);
  end
  [value, isterminal, direction] = odeEvaluateEventObject(args{:});
end
%=============================================================================
