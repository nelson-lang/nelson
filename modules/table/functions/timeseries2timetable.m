%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TT = timeseries2timetable(varargin)
  % Each timeseries becomes one variable, named after the timeseries (or
  % Data), made unique; all inputs must share the same time vector. A
  % single input may be a timeseries array, taken in column order.
  narginchk(1, Inf);
  tsList = timeseriesList(varargin);
  data = cell(1, length(tsList));
  names = cell(1, length(tsList));
  for k = 1:length(tsList)
    ts = tsList{k};
    if k > 1 && ~sameTimeVector(tsList{1}, ts)
      error('Nelson:table:timeseriesTimeMismatch', _('All inputs must have the same time vector, TimeInfo Units, and TimeInfo StartDate. Separately convert each timeseries to a timetable, and then use the SYNCHRONIZE function to combine the timetables.'));
    end
    data{k} = dataWithSamplesInRows(ts);
    names{k} = ts.Name;
    if isempty(names{k})
      names{k} = 'Data';
    end
  end
  names = nelson.lang.makeUniqueStrings(names);
  TT = timetable(data{:}, 'RowTimes', makeRowTimes(tsList{1}), 'VariableNames', names);
end
%=============================================================================
function tsList = timeseriesList(args)
  % timeseries overloads numel and () indexing on its samples, so the
  % elements of an array are reached through the builtins.
  for k = 1:length(args)
    if ~istimeseries(args{k})
      error('Nelson:table:invalidType:inputATimeseriesObject', 'Input must be a timeseries object.');
    end
  end
  if length(args) > 1
    for k = 1:length(args)
      if ~isscalar(args{k})
        error('Nelson:table:timeseriesNonscalarInput', _('If multiple inputs are given to timeseries2timetable, they all must be scalar timeseries objects.'));
      end
    end
    tsList = args;
    return
  end
  n = builtin('numel', args{1});
  if n == 0
    error('Nelson:table:emptyTimeseriesArray', _('timeseries array input must be non-empty.'));
  end
  tsList = cell(1, n);
  for k = 1:n
    tsList{k} = builtin('subsref', args{1}, substruct('()', {k}));
  end
end
%=============================================================================
function tf = sameTimeVector(a, b)
  tf = isequal(a.Time(:), b.Time(:)) && strcmp(a.TimeInfo.Units, b.TimeInfo.Units) ...
    && isequal(a.TimeInfo.StartDate, b.TimeInfo.StartDate);
end
%=============================================================================
function data = dataWithSamplesInRows(ts)
  data = ts.Data;
  if isvector(data)
    data = data(:);
  elseif ~ts.IsTimeFirst
    order = [ndims(data), 1:ndims(data) - 1];
    data = permute(data, order);
  end
end
%=============================================================================
function rowTimes = makeRowTimes(ts)
  time = ts.Time(:);
  if ~isempty(ts.TimeInfo.StartDate)
    serial = datenum(ts.TimeInfo.StartDate) + time .* unitDays(ts.TimeInfo.Units);
    rowTimes = datetime(serial, 'ConvertFrom', 'datenum');
  else
    rowTimes = seconds(time .* unitSeconds(ts.TimeInfo.Units));
  end
end
%=============================================================================
function scale = unitSeconds(units)
  switch lower(units)
    case {'second', 'seconds', 'sec', 'secs'}
      scale = 1;
    case {'minute', 'minutes', 'min', 'mins'}
      scale = 60;
    case {'hour', 'hours', 'hr', 'hrs'}
      scale = 3600;
    case {'day', 'days'}
      scale = 86400;
    otherwise
      scale = 1;
  end
end
%=============================================================================
function scale = unitDays(units)
  scale = unitSeconds(units) ./ 86400;
end
%=============================================================================
