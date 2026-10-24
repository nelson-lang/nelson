%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [rows, eventRows] = timetableEventMatches(rowTimes, eventTimes, eventEnds)
  % Pairs (timetable row, event table row) of the events happening at each
  % row time, in row order then event order. A row without event is paired
  % with 0. An event without end matches the rows at its time, an event
  % with an end the rows in [time, end).
  t = timeNumbers(rowTimes);
  s = timeNumbers(eventTimes);
  hasEnds = ~isempty(eventEnds);
  if hasEnds
    e = timeNumbers(eventEnds);
  end
  matchedRows = cell(numel(s), 1);
  matchedEvents = cell(numel(s), 1);
  for k = 1:numel(s)
    if hasEnds
      found = find(t >= s(k) & t < e(k));
    else
      found = find(t == s(k));
    end
    matchedRows{k} = found(:);
    matchedEvents{k} = k * ones(numel(found), 1);
  end
  rows = vertcat(zeros(0, 1), matchedRows{:});
  eventRows = vertcat(zeros(0, 1), matchedEvents{:});
  alone = true(numel(t), 1);
  alone(rows) = false;
  rows = [rows; find(alone)];
  eventRows = [eventRows; zeros(nnz(alone), 1)];
  [~, order] = sort(rows * (numel(s) + 1) + eventRows);
  rows = rows(order);
  eventRows = eventRows(order);
end
%=============================================================================
function x = timeNumbers(times)
  if isa(times, 'datetime')
    x = datenum(times(:));
  else
    x = seconds(times(:));
  end
end
%=============================================================================
