%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef event
  properties
    Name
    Time
    Units
    StartDate
    UserData
  end

  methods
    function obj = event(name, time, varargin)
      defaults = localEventDefaults();
      obj.Name = defaults.Name;
      obj.Time = defaults.Time;
      obj.Units = defaults.Units;
      obj.StartDate = defaults.StartDate;
      obj.UserData = defaults.UserData;
      if nargin >= 1
        obj.Name = name;
      end
      if nargin >= 2
        obj.Time = time;
      end
      if nargin > 2
        obj = obj.set(varargin{:});
      end
    end

    function display(obj)
      disp('tsdata.event');
      disp(' ');
      disp(obj);
    end

    function tf = eq(a, b)
      if ~isa(a, 'tsdata.event') || ~isa(b, 'tsdata.event')
        tf = false;
        return
      end
      sameText = strcmp(a.Name, b.Name) && strcmp(a.Units, b.Units);
      sameTime = isequal(a.Time, b.Time);
      sameStart = strcmp(a.StartDate, b.StartDate);
      tf = sameText && sameTime && sameStart;
    end

    function [event, index] = findEvent(obj, name, varargin)
      name = localEventText(name, 'Event name');
      index = [];
      event = [];
      for k = 1:numel(obj)
        if strcmpi(obj(k).Name, name)
          index = k;
          event = obj(k);
          return
        end
      end
    end

    function value = get(obj, varargin)
      value = localEventGet(obj, varargin{:});
    end

    function obj = set(obj, varargin)
      if rem(nargin - 1, 2) ~= 0
        error('Nelson:time:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
      end
      for k = 1:2:length(varargin)
        name = localEventText(varargin{k}, 'Property name');
        obj = localEventAssign(obj, name, varargin{k + 1});
      end
    end

    function value = getTimeStr(obj)
      value = localEventTimeString(obj);
    end
  end
end
%=============================================================================
function defaults = localEventDefaults()
  defaults = struct();
  defaults.Name = '';
  defaults.Time = [];
  defaults.Units = 'seconds';
  defaults.StartDate = '';
  defaults.UserData = [];
end
%=============================================================================
function obj = localEventAssign(obj, name, value)
  switch lower(name)
    case 'name'
      obj.Name = value;
    case 'time'
      obj.Time = value;
    case 'units'
      obj.Units = value;
    case 'startdate'
      obj.StartDate = value;
    case 'userdata'
      obj.UserData = value;
    otherwise
      error('Nelson:time:unknownEventProperty', 'Unknown %s property.', 'event');
  end
end
%=============================================================================
function value = localEventTimeString(obj)
  if isempty(obj.Time)
    value = '';
  elseif isempty(obj.StartDate)
    value = [num2str(obj.Time), ' ', obj.Units];
  else
    offset = obj.Time .* localEventUnitDays(obj.Units);
    value = datestr(datenum(obj.StartDate) + offset);
  end
end
%=============================================================================
function value = localEventGet(obj, varargin)
  names = properties(obj);
  if nargin == 1
    value = cell2struct(cell(size(names)), names);
    for k = 1:length(names)
      value.(names{k}) = obj.(names{k});
    end
    return
  end
  if nargin < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nargin > 2
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  name = localEventText(varargin{1}, 'Property name');
  match = strcmpi(name, names);
  if ~any(match)
    error('Nelson:time:unknownEventProperty', 'Unknown %s property.', 'event');
  end
  value = obj.(names{find(match, 1)});
end
%=============================================================================
function value = localEventText(value, name)
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    error('Nelson:time:mustBeText', '%s must be text.', name);
  end
end
%=============================================================================
function scale = localEventUnitDays(units)
  units = lower(localEventText(units, 'Units'));
  switch units
    case {'weeks', 'week'}
      scale = 7;
    case {'days', 'day'}
      scale = 1;
    case {'hours', 'hour'}
      scale = 1 / 24;
    case {'minutes', 'minute'}
      scale = 1 / (24 * 60);
    case {'seconds', 'second'}
      scale = 1 / (24 * 60 * 60);
    case {'milliseconds', 'millisecond'}
      scale = 1 / (24 * 60 * 60 * 1000);
    otherwise
      scale = 1 / (24 * 60 * 60);
  end
end
%=============================================================================
