%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = plot(varargin)
  [ax, obj, args] = parsePlotInputs(varargin{:});
  if ~isscalar(obj)
    error('Nelson:time:inputScalarTimeseries', 'Input must be a scalar timeseries.');
  end
  if isempty(obj.Time) || isempty(obj.Data)
    error('Nelson:time:timeseriesContainTimeData', 'Timeseries must contain time and data.');
  end
  if ~(isnumeric(obj.Data) || islogical(obj.Data))
    error('Nelson:time:timeseriesDataNumericLogical', 'Timeseries data must be numeric or logical.');
  end
  x = obj.Time;
  if ~isempty(obj.TimeInfo.StartDate)
    x = datenum(obj.TimeInfo.StartDate) + x .* timeseriesUnitDays(obj.TimeInfo.Units);
  end
  y = timeseriesDataAsColumns(obj);
  if strcmpi(getinterpmethod(obj), 'zoh')
    h = stairs(ax, x, y, args{:});
  else
    h = plot(ax, x, y, args{:});
  end
  applyTimeseriesLabels(ax, obj);
  holdState = ishold(ax);
  hold(ax, 'on');
  events = obj.Events;
  for k = 1:numel(events)
    currentEvent = timeseriesEventAt(events, k);
    tx = currentEvent.Time;
    if ~isempty(obj.TimeInfo.StartDate)
      tx = datenum(obj.TimeInfo.StartDate) + tx .* timeseriesUnitDays(obj.TimeInfo.Units);
    end
    yl = ylim(ax);
    plot(ax, [tx tx], yl, ':', 'Visible', 'on');
  end
  if ~holdState
    hold(ax, 'off');
  end
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function applyTimeseriesLabels(ax, obj)
  seriesName = obj.Name;
  if isempty(seriesName)
    seriesName = 'Data';
  end
  title(ax, ['Time Series Plot:', seriesName]);
  if isempty(obj.DataInfo.Units)
    ylabel(ax, seriesName);
  else
    ylabel(ax, [seriesName, ' (', obj.DataInfo.Units, ')']);
  end
  if isempty(obj.TimeInfo.StartDate)
    xlabel(ax, ['Time (', obj.TimeInfo.Units, ')']);
  else
    xlabel(ax, 'Time');
  end
end
%=============================================================================
function [ax, obj, args] = parsePlotInputs(varargin)
  if nargin >= 2 && isgraphics(varargin{1}, 'axes')
    ax = varargin{1};
    obj = varargin{2};
    args = varargin(3:end);
  else
    ax = gca();
    obj = varargin{1};
    args = varargin(2:end);
  end
end
%=============================================================================
