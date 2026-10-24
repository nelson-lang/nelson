%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Puts the requested axis ('x', 'y' or 'z') into date mode after data that was
% originally a datetime array has been plotted as serial date numbers. The
% ruler keeps numeric coordinates but the tick labels are formatted as dates,
% so the axis reads as a temporal axis.
function applyDatetimeAxis(parent, whichAxis)
  if isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  else
    ax = parent;
  end
  if isempty(ax) || ~isgraphics(ax, 'axes')
    return
  end
  % Visible labels: datetick pins the ticks/labels to manual date strings (as
  % today). This must run first so the manual label mode is set before the
  % format is stored.
  datetick(ax, whichAxis);
  % Persistent date mode: flip the matching axis into datetime mode and store a
  % sensible display format. The axis surfaces both through its ruler
  % (Axes.XAxis / YAxis / ZAxis), which then reports a DatetimeRuler type/class
  % and this TickLabelFormat, persistently across later updateState re-runs.
  upperAxis = upper(whichAxis);
  scaleKindProperty = [upperAxis, 'AxisScaleKind'];
  formatProperty = [upperAxis, 'TickLabelFormat'];
  limits = get(ax, [upperAxis, 'Lim']);
  set(ax, scaleKindProperty, 'datetime');
  set(ax, formatProperty, defaultDatetimeFormat(limits(2) - limits(1)));
end
%=============================================================================
% Mirrors datetick's span-based default so the stored format matches the visible
% labels. Kept in step with datetick.m's defaultDateFormat.
function fmt = defaultDatetimeFormat(spanDays)
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
