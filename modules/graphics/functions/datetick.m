%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function datetick(varargin)
  narginchk(0, 6);
  nargoutchk(0, 0);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseAxis(args);
  tickAxis = 'x';
  dateFormat = [];
  keepLimits = false;
  keepTicks = false;
  for k = 1:numel(args)
    arg = args{k};
    if ischar(arg) && any(strcmpi(arg, {'x', 'y', 'z'}))
      tickAxis = lower(arg);
    elseif ischar(arg) && strcmpi(arg, 'keeplimits')
      keepLimits = true;
    elseif ischar(arg) && strcmpi(arg, 'keepticks')
      keepTicks = true;
    elseif (ischar(arg) && ~isempty(arg)) || (isnumeric(arg) && isscalar(arg))
      dateFormat = arg;
    else
      error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
  end
  upperAxis = upper(tickAxis);
  limProperty = [upperAxis, 'Lim'];
  tickProperty = [upperAxis, 'Tick'];
  labelProperty = [upperAxis, 'TickLabel'];
  limits = get(ax, limProperty);
  ticks = get(ax, tickProperty);
  if ~keepTicks
    ticks = ticks(ticks >= limits(1) & ticks <= limits(2));
    if numel(ticks) < 2
      ticks = linspace(limits(1), limits(2), 6);
    end
  end
  ticks = ticks(:)';
  if isempty(ticks)
    return
  end
  if isempty(dateFormat)
    dateFormat = defaultDateFormat(max(ticks) - min(ticks));
  end
  labels = datestr(ticks(:), dateFormat);
  set(ax, tickProperty, ticks);
  set(ax, labelProperty, cellstr(labels));
  if keepLimits
    set(ax, limProperty, limits);
  end
end
%=============================================================================
function [ax, args] = parseAxis(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args = args(2:end);
  else
    ax = gca();
  end
end
%=============================================================================
function fmt = defaultDateFormat(spanDays)
  if spanDays >= 730
    fmt = 'yyyy';
  elseif spanDays >= 62
    fmt = 'mmm-yy';
  elseif spanDays >= 2
    fmt = 'mm/dd';
  elseif spanDays >= 1 / 24
    fmt = 'HH:MM';
  else
    fmt = 'HH:MM:SS';
  end
end
%=============================================================================
