%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [rows, newTimes] = timetableResolveRows(rowTimes, sub, allowNew)
  % Row numbers selected by ':', a logical mask, numbers, a timerange, a
  % withtol or row times (text is converted to the row time type). A time
  % selects all the rows at that time, none when it is not a row time; with
  % allowNew (an assignment) it selects the first of them, or a new row
  % after the last one, and newTimes lists these new times in the order of
  % their rows.
  newTimes = rowTimes([]);
  if ischar(sub) && strcmp(sub, ':')
    rows = 1:timetableTimeCount(rowTimes);
    return
  end
  [sub, rows, newTimes, done] = labelOrTextSubscript(rowTimes, sub, nargin > 2 && allowNew);
  if done
    return
  end
  if islogical(sub)
    rows = find(sub);
    rows = rows(:)';
  elseif isnumeric(sub)
    rows = sub(:)';
  elseif isa(sub, 'timerange')
    rows = find(timerange.contains(sub, rowTimes));
    rows = rows(:)';
  elseif isa(sub, 'withtol')
    rows = withtol.rows(sub, rowTimes);
  elseif (isa(sub, 'datetime') || isa(sub, 'duration')) && ~strcmp(class(sub), class(rowTimes))
    kind = class(rowTimes);
    error(['Nelson:timetable:InvalidRowSubscripts', upper(kind(1)), kind(2:end)], ...
      _('Timetable row subscript must be a numeric, logical, or %s array, or a timerange subscripter.'), kind);
  elseif (isa(sub, 'datetime') || isa(sub, 'duration')) && nargin > 2 && allowNew
    [rows, newTimes] = assignedTimeRows(rowTimes, sub);
  elseif isa(sub, 'datetime') || isa(sub, 'duration')
    rows = withtol.rows(withtol(sub, seconds(0)), rowTimes);
  else
    error('Nelson:timetable:invalidSubscriptType', 'Invalid %s subscript type.', 'row');
  end
end
%=============================================================================
function [sub, rows, newTimes, done] = labelOrTextSubscript(rowTimes, sub, allowNew)
  % labels are resolved here; text becomes row times of the row time type
  done = isa(sub, 'labels');
  rows = [];
  newTimes = rowTimes([]);
  if done
    [rows, newTimes] = labelRows(rowTimes, labels.values(sub), allowNew);
  elseif ischar(sub) || isstring(sub) || iscellstr(sub)
    sub = textTimes(rowTimes, sub);
  end
end
%=============================================================================
function times = textTimes(rowTimes, text)
  if isa(rowTimes, 'datetime')
    times = datetime(text);
  elseif isa(rowTimes, 'duration')
    times = duration(text);
  else
    error('Nelson:timetable:textSubscriptNumericRowTimes', ...
      'Text cannot select rows of a timetable with numeric row times: use labels.');
  end
end
%=============================================================================
function [rows, newTimes] = labelRows(rowTimes, values, allowNew)
  % rows whose row times equal the label values (datetime and duration
  % labels select like times)
  if isa(values, 'datetime') || isa(values, 'duration')
    [rows, newTimes] = timetableResolveRows(rowTimes, values, allowNew);
    return
  end
  if ~(isa(rowTimes, 'double') || isa(rowTimes, 'single'))
    error('Nelson:timetable:labelsTypeMismatch', ...
      'Numeric labels select rows of a timetable with numeric row times only.');
  end
  if allowNew
    [rows, newTimes] = assignedLabelRows(rowTimes, values);
    return
  end
  newTimes = rowTimes([]);
  matched = arrayfun(@(v) reshape(find(rowTimes == v), 1, []), values(:)', 'UniformOutput', false);
  rows = [zeros(1, 0), matched{:}];
end
%=============================================================================
function [rows, newTimes] = assignedLabelRows(rowTimes, values)
  % first row at each label value, or a new row after the last one
  newTimes = rowTimes([]);
  rows = zeros(1, numel(values));
  for k = 1:numel(values)
    idx = find(rowTimes == values(k), 1);
    if isempty(idx)
      idx = find(newTimes == values(k), 1);
      if isempty(idx)
        newTimes(end + 1, 1) = values(k);
        idx = numel(newTimes);
      end
      idx = numel(rowTimes) + idx;
    end
    rows(k) = idx;
  end
end
%=============================================================================
function n = timetableTimeCount(value)
  if isa(value, 'datetime')
    n = numel(datenum(value));
  elseif isa(value, 'duration')
    n = numel(seconds(value));
  else
    n = numel(value);
  end
end
%=============================================================================
function [rows, newTimes] = assignedTimeRows(rowTimes, sub)
  newTimes = rowTimes([]);
  nRows = timetableTimeCount(rowTimes);
  rows = zeros(1, timetableTimeCount(sub));
  for k = 1:length(rows)
    idx = withtol.rows(withtol(sub(k), seconds(0)), rowTimes);
    if isempty(idx)
      idx = withtol.rows(withtol(sub(k), seconds(0)), newTimes);
      if isempty(idx)
        newTimes(end + 1, 1) = sub(k);
        idx = timetableTimeCount(newTimes);
      end
      idx = nRows + idx;
    end
    rows(k) = idx(1);
  end
end
%=============================================================================
