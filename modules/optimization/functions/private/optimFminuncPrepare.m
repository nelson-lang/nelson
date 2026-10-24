%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function data = optimFminuncPrepare(fun, x0, opts)
  if isempty(x0)
    error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'Initial point');
  end
  if ~isnumeric(x0) || ~isreal(x0)
    error('Nelson:optimization:invalidInitialPoint', '%s must be a real numeric array.', 'Initial point');
  end
  if ~(isa(fun, 'function_handle') || ischar(fun) || isstring(fun))
    error('Nelson:optimization:invalidObjective', 'Objective must be a function handle or a function name.');
  end
  algorithm = lower(char(opts.Algorithm));
  if strcmp(algorithm, 'default')
    algorithm = 'quasi-newton';
  end
  if ~any(strcmp(algorithm, {'quasi-newton', 'trust-region'}))
    error('Nelson:optim:invalidOptionValue', 'Invalid value for "%s".', 'Algorithm');
  end
  validateTextChoice(opts.Display, {'off', 'none', 'final', 'final-detailed', 'notify', ...
    'notify-detailed', 'iter', 'iter-detailed'}, sprintf(_('Invalid value for "%s".'), 'Display'));
  validateTextChoice(opts.Diagnostics, {'on', 'off'}, sprintf(_('Invalid value for "%s".'), 'Diagnostics'));
  validateTextChoice(opts.FiniteDifferenceType, {'forward', 'central'}, sprintf(_('Invalid value for "%s".'), 'FiniteDifferenceType'));
  if strcmp(algorithm, 'trust-region') && ~hasGradient(opts)
    algorithm = 'quasi-newton';
  end
  data.fun = fun;
  data.x0 = double(x0(:));
  data.xShape = size(x0);
  data.algorithm = algorithm;
end
%=============================================================================
function tf = hasGradient(opts)
  tf = opts.SpecifyObjectiveGradient || strcmpi(opts.GradObj, 'on');
end
%=============================================================================
function validateTextChoice(value, choices, message)
  value = lower(char(value));
  if ~any(strcmp(value, choices))
    error('Nelson:optimization:invalidValue', message);
  end
end
%=============================================================================
