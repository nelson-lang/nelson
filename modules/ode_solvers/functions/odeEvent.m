%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef odeEvent
  properties
    EventFcn = []
    Direction = 'both'
    Response = 'proceed'
    CallbackFcn = []
  end

  methods
    function obj = odeEvent(varargin)
      if nargin == 1 && isa(varargin{1}, 'function_handle')
        obj.EventFcn = varargin{1};
        return
      end
      if mod(nargin, 2) ~= 0
        error('Nelson:ode_solvers:nameValueArgumentsExpected', 'Name-value arguments expected.');
      end
      for k = 1:2:nargin
        name = char(varargin{k});
        obj = odeAssignEventProperty(obj, name, varargin{k + 1});
      end
    end

    function obj = set.EventFcn(obj, value)
      if ~(isempty(value) || isa(value, 'function_handle'))
        error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
      end
      obj.EventFcn = value;
    end

    function obj = set.Direction(obj, value)
      obj.Direction = odeValidateEventChoice(value, 'Direction', {'both', 'increasing', 'decreasing'});
    end

    function obj = set.Response(obj, value)
      obj.Response = odeValidateEventChoice(value, 'Response', {'proceed', 'stop', 'callback'});
    end

    function obj = set.CallbackFcn(obj, value)
      if ~(isempty(value) || isa(value, 'function_handle'))
        error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
      end
      obj.CallbackFcn = value;
    end
  end
end
%=============================================================================
function obj = odeAssignEventProperty(obj, name, value)
  switch lower(name)
    case 'eventfcn'
      if ~(isempty(value) || isa(value, 'function_handle'))
        error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
      end
      obj.EventFcn = value;
    case 'direction'
      obj.Direction = odeValidateEventChoice(value, 'Direction', {'both', 'increasing', 'decreasing'});
    case 'response'
      obj.Response = odeValidateEventChoice(value, 'Response', {'proceed', 'stop', 'callback'});
    case 'callbackfcn'
      if ~(isempty(value) || isa(value, 'function_handle'))
        error('Nelson:ode_solvers:functionHandleExpected', '%s must be a function handle.', 'Value');
      end
      obj.CallbackFcn = value;
    otherwise
      error('Nelson:ode_solvers:unknownEventProperty', 'Unknown %s property.', 'event');
  end
end
%=============================================================================
function value = odeValidateEventChoice(value, name, choices)
  if isstring(value)
    if isscalar(value)
      value = char(value);
    else
      values = cell(numel(value), 1);
      for k = 1:numel(value)
        values{k} = odeValidateEventChoiceOne(char(value(k)), name, choices);
      end
      value = values;
      return
    end
  end
  if iscell(value)
    values = cell(numel(value), 1);
    for k = 1:numel(value)
      values{k} = odeValidateEventChoiceOne(value{k}, name, choices);
    end
    value = values;
    return
  end
  if ischar(value) && size(value, 1) > 1
    values = cell(size(value, 1), 1);
    for k = 1:size(value, 1)
      values{k} = odeValidateEventChoiceOne(deblank(value(k, :)), name, choices);
    end
    value = values;
    return
  end
  value = odeValidateEventChoiceOne(value, name, choices);
end
%=============================================================================
function value = odeValidateEventChoiceOne(value, name, choices)
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    error('Nelson:ode_solvers:invalidValue', '%s has an invalid value.', name);
  end
  for k = 1:length(choices)
    if strcmpi(value, choices{k})
      value = choices{k};
      return
    end
  end
  error('Nelson:ode_solvers:invalidValue', '%s has an invalid value.', name);
end
%=============================================================================
