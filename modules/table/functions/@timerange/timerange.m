%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef timerange
  properties (Hidden)
    StartValue = -Inf
    EndValue = Inf
    IncludeStart = true
    IncludeEnd = false
    TimeKind = ''
    % the limits as given (datetime, duration or '-inf'/'inf' text), for display
    Limits = {}
    % time zone of the datetime limits; their values are UTC instants (posixtime)
    TimeZoneValue = ''
    % limits given as text only take the time zone of the row times
    FollowsRowTimesZone = false
  end

  methods
    function obj = timerange(varargin)
      narginchk(2, 3);
      unit = '';
      endpoints = varargin(1:2);
      if nargin == 3 && timerange.isUnit(varargin{3})
        unit = varargin{3};
      elseif nargin == 3
        [obj.IncludeStart, obj.IncludeEnd] = timerange.intervalFlags(varargin{3});
      elseif timerange.isUnit(varargin{2})
        unit = varargin{2};
        endpoints = varargin(1);
      end
      n = numel(endpoints);
      values = cell(1, n);
      kinds = cell(1, n);
      limits = cell(1, n);
      for k = 1:n
        [values{k}, kinds{k}, limits{k}] = timerange.endpointValue(endpoints{k});
      end
      obj.TimeKind = timerange.mergeKinds(kinds);
      obj.TimeZoneValue = checkTimeZones(endpoints);
      obj.FollowsRowTimesZone = ~any(cellfun(@(x) isa(x, 'datetime'), endpoints));
      [values, limits] = datetimeInstants(values, limits, obj.TimeZoneValue);
      if isempty(unit)
        obj.StartValue = values{1};
        obj.EndValue = values{2};
        obj.Limits = limits;
      else
        if strcmp(obj.TimeKind, 'duration')
          error('Nelson:timerange:UnitOfTimeTypesMismatch', 'Durations and event filters are not supported when specifying a unit of time for a time range. Use datetimes when specifying unit of time.');
        end
        obj.TimeKind = 'datetime';
        [obj.StartValue, obj.EndValue, obj.Limits] = timerange.periodBounds(values, limits, timerange.unitName(unit));
      end
    end

    function display(obj, varargin)
      if nargin == 1
        name = inputname(1);
      else
        name = varargin{1};
      end
      if ~isempty(name)
        looseBlankLine();
        fprintf('%s = \n', name);
        looseBlankLine();
      end
      disp(obj);
    end

    function disp(obj)
      fprintf('\ttimetable timerange subscript:\n');
      looseBlankLine();
      reason = emptyReason(obj);
      if ~isempty(reason)
        fprintf('\t\tSelect no rows because %s.\n', reason);
        looseBlankLine();
        return
      end
      kinds = {'open', 'half-open', 'closed'};
      kind = kinds{1 + obj.IncludeStart + obj.IncludeEnd};
      fprintf('\t\tSelect timetable rows with times in the %s interval:\n', kind);
      if numel(obj.Limits) == 2
        labels = {boundLabel('Starting at', obj.IncludeStart), boundLabel('Ending at', obj.IncludeEnd)};
        texts = {limitText(obj.Limits{1}, obj.Limits{2}), limitText(obj.Limits{2}, obj.Limits{1})};
        width = max(cellfun(@length, labels));
        for k = 1:2
          padding = blanks(width - length(labels{k}) + 1);
          fprintf('\t\t  %s%s%s\n', labels{k}, padding, texts{k});
        end
      end
      looseBlankLine();
    end
  end

  methods (Static, Hidden)
    function mask = contains(obj, times)
      if isa(times, 'datetime')
        obj = rowTimesZoneRange(obj, times);
        values = posixtime(times);
      elseif isa(times, 'duration')
        values = double(times);
      else
        error('Nelson:timetable:invalidRowTimes', 'Timetable row times must be datetime or duration values.');
      end
      if obj.IncludeStart
        left = values >= obj.StartValue;
      else
        left = values > obj.StartValue;
      end
      if obj.IncludeEnd
        right = values <= obj.EndValue;
      else
        right = values < obj.EndValue;
      end
      mask = left & right;
    end

    function tf = isUnit(value)
      tf = isTextScalar(value) && any(strcmp(timerange.unitName(value), ...
        {'year', 'quarter', 'month', 'week', 'day', 'hour', 'minute', 'second'}));
    end

    function [value, kind, limit] = endpointValue(input)
      % kind: 'datetime', 'duration', 'inf' ('-inf'/'inf' text or numeric
      % -Inf/Inf, either time type) or 'nan' (numeric NaN, a duration).
      limit = input;
      if isa(input, 'datetime') || isa(input, 'duration')
        if numel(input) ~= 1
          error('Nelson:timerange:NonScalarInput', 'Arguments must be scalars.');
        end
      elseif isTextScalar(input)
        [limit, value] = textEndpoint(input);
      elseif (isa(input, 'double') || isa(input, 'single')) && isscalar(input) ...
          && isreal(input) && ~isfinite(input)
        limit = double(input);
        value = limit;
      else
        error('Nelson:timerange:InvalidTimes', 'Time range endpoints must be datetime or duration scalars, eventfilter, or text representing datetime or duration scalars.');
      end
      if isa(limit, 'datetime')
        value = NaN; % the instant, once the time zone of the range is known
        kind = 'datetime';
      elseif isa(limit, 'duration')
        value = double(limit);
        kind = 'duration';
      elseif isnan(value)
        kind = 'nan';
      else
        kind = 'inf';
      end
    end

    function kind = mergeKinds(kinds)
      typed = unique(kinds(ismember(kinds, {'datetime', 'duration'})));
      hasNaN = any(strcmp(kinds, 'nan'));
      if numel(typed) > 1 || (hasNaN && any(strcmp(typed, 'datetime')))
        error('Nelson:timerange:InputTypesMismatch', 'Time range endpoints must both be datetimes, both be durations, or both be eventfilters.');
      end
      if hasNaN
        kind = 'duration';
      elseif ~isempty(typed)
        kind = typed{1};
      elseif numel(kinds) == 2
        error('Nelson:timerange:AmbiguousTimes', 'The endpoints cannot be unambiguously interpreted as datetimes or durations. Convert text or numeric inputs to datetime or duration.');
      else
        kind = '';
      end
    end

    function [startValue, endValue, limits] = periodBounds(values, limits, unit)
      % the period of each datetime endpoint; an infinite endpoint stays infinite.
      startValue = values{1};
      endValue = values{end};
      limits = limits([1, end]);
      if isa(limits{1}, 'datetime')
        limits{1} = dateshift(limits{1}, 'start', unit);
        startValue = posixtime(limits{1});
      end
      if isa(limits{2}, 'datetime')
        limits{2} = timerange.addOneUnit(dateshift(limits{2}, 'start', unit), unit);
        endValue = posixtime(limits{2});
      end
    end

    function unit = unitName(unit)
      unit = lower(char(unit));
      if endsWith(unit, 's')
        unit = unit(1:end - 1);
      end
    end

    function out = addOneUnit(t, unit)
      switch unit
        case 'year'
          out = t + calyears(1);
        case 'quarter'
          out = t + calquarters(1);
        case 'month'
          out = t + calmonths(1);
        case 'week'
          out = t + calweeks(1);
        case 'day'
          out = t + caldays(1);
        case 'hour'
          out = t + hours(1);
        case 'minute'
          out = t + minutes(1);
        case 'second'
          out = t + seconds(1);
      end
    end

    function [includeStart, includeEnd] = intervalFlags(intervalType)
      if isTextScalar(intervalType)
        intervalType = lower(char(intervalType));
      else
        intervalType = '';
      end
      switch intervalType
        case {'openright', 'closedleft'}
          includeStart = true;
          includeEnd = false;
        case {'openleft', 'closedright'}
          includeStart = false;
          includeEnd = true;
        case 'open'
          includeStart = false;
          includeEnd = false;
        case 'closed'
          includeStart = true;
          includeEnd = true;
        otherwise
          error('Nelson:timerange:InvalidIntervalType', ...
            'Specify third argument either as interval type (''openright'', ''openleft'', ''open'', ''closed'') or as unit of time (''years'', ''quarters'', ''months'', ''weeks'', ''days'', ''hours'', ''minutes'', ''seconds'').');
      end
    end
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = (ischar(value) && (isrow(value) || isempty(value))) || (isstring(value) && isscalar(value));
end
%=============================================================================
function [limit, value] = textEndpoint(input)
  % '-inf'/'inf' text stays text; 'hh:mm:ss' or 'dd:hh:mm:ss' names a
  % duration; any other text names a datetime.
  text = strtrim(char(input));
  value = [];
  if any(strcmpi(text, {'inf', '+inf', '-inf'}))
    limit = text;
    value = Inf;
    if text(1) == '-'
      value = -Inf;
    end
  elseif ~isempty(regexp(text, '^[-+]?\d+(:\d+){2,3}(\.\d*)?$', 'once'))
    limit = duration(text);
  else
    try
      limit = datetime(text);
      recognized = ~isnat(limit) || strcmpi(text, 'NaT');
    catch
      recognized = false;
    end
    if ~recognized
      error('Nelson:datetime:InvalidTextInput', 'Unable to recognize date/time format of ''%s''. Convert text to datetime or duration.', text);
    end
  end
end
%=============================================================================
function zone = checkTimeZones(endpoints)
  % Time zone of the datetime limits: two datetimes, one zoned and one
  % unzoned (NaT is unzoned), do not combine.
  isDatetime = cellfun(@(x) isa(x, 'datetime'), endpoints);
  zones = cellfun(@(x) x.TimeZone, endpoints(isDatetime), 'UniformOutput', false);
  zone = '';
  if ~isempty(zones)
    zone = zones{1};
  end
  if numel(zones) == 2 && xor(isempty(zones{1}), isempty(zones{2}))
    error('Nelson:timerange:TimeZonesMismatch', 'Unable to combine a datetime that has a time zone with an unzoned datetime.');
  end
end
%=============================================================================
function [values, limits] = datetimeInstants(values, limits, zone)
  % Datetime limits as UTC instants; text limits read in the range time zone.
  for k = 1:numel(limits)
    if isa(limits{k}, 'datetime')
      limit = limits{k};
      if isempty(limit.TimeZone) && ~isempty(zone)
        limit.TimeZone = zone;
      end
      limits{k} = limit;
      values{k} = posixtime(limit);
    end
  end
end
%=============================================================================
function obj = rowTimesZoneRange(obj, times)
  % The range compared with datetime row times: text limits are read in the
  % time zone of the row times; a zoned range never selects unzoned row times.
  zone = times.TimeZone;
  hasDatetimeLimit = any(cellfun(@(x) isa(x, 'datetime'), obj.Limits));
  if ~hasDatetimeLimit || strcmp(obj.TimeZoneValue, zone)
    return
  end
  if ~obj.FollowsRowTimesZone && xor(isempty(obj.TimeZoneValue), isempty(zone))
    error('Nelson:datetime:IncompatibleTZ', 'Cannot combine or compare a datetime array with a time zone with one without a time zone.');
  end
  if obj.FollowsRowTimesZone
    values = {obj.StartValue, obj.EndValue};
    [values, limits] = datetimeInstants(values, obj.Limits, zone);
    obj.Limits = limits;
    obj.StartValue = values{1};
    obj.EndValue = values{2};
    obj.TimeZoneValue = zone;
  end
end
%=============================================================================
function reason = emptyReason(obj)
  reason = '';
  if isnan(obj.StartValue) || isnan(obj.EndValue)
    reason = 'an endpoint is a NaN/NaT';
  elseif obj.StartValue > obj.EndValue ...
      || (obj.StartValue == obj.EndValue && ~(obj.IncludeStart && obj.IncludeEnd))
    reason = 'the left endpoint is at or after the right endpoint';
  end
end
%=============================================================================
function label = boundLabel(prefix, isIncluded)
  if isIncluded
    label = [prefix, ', including:'];
  else
    label = [prefix, ', but excluding:'];
  end
end
%=============================================================================
function text = limitText(limit, other)
  % datetimes with their time of day; an infinite limit takes the unit of
  % the other limit when it is a duration.
  if isa(limit, 'datetime')
    limit.Format = 'dd-MMM-yyyy HH:mm:ss';
    text = char(limit);
  elseif isa(limit, 'duration')
    text = char(limit);
  else
    value = infiniteValue(limit);
    if isa(other, 'duration')
      value = seconds(value);
      value.Format = other.Format;
      text = char(value);
    else
      text = num2str(value);
    end
  end
end
%=============================================================================
function value = infiniteValue(limit)
  if isnumeric(limit)
    value = limit;
  elseif strcmp(limit(1), '-')
    value = -Inf;
  else
    value = Inf;
  end
end
%=============================================================================
function looseBlankLine()
  % Separator line of the loose line spacing, absent from the compact one.
  currentFormat = format();
  if strcmp(currentFormat.LineSpacing, 'loose')
    fprintf('\n');
  end
end
%=============================================================================
