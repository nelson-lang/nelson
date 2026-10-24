%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function strs = timetableEventColumn(strs, events, rowTimes)
  % Display texts of a timetable with its event table: an unlabelled first
  % column shows, on each row, the label of the event happening at its row
  % time, "<k events>" when several events match it and "<1 event>" when
  % the event has no label. The row times stay left-aligned under their
  % header.
  if ~isa(events, 'eventtable') || size(strs, 1) < 2
    return
  end
  width = max(strlength(strs(:, 1)));
  strs(2:end, 1) = pad(strs(2:end, 1), width, 'right');
  props = subsref(events, struct('type', '.', 'subs', 'Properties'));
  [rows, eventRows] = timetableEventMatches(rowTimes, props.RowTimes, timetableEventEnds(events));
  matched = eventRows > 0;
  counts = accumarray(rows(matched), 1, [numel(rowTimes), 1]);
  labels = strings(numel(rowTimes), 1);
  labels(counts > 1) = "<" + string(counts(counts > 1)) + " events>";
  texts = labelTexts(events);
  single = find(counts == 1);
  [~, where] = ismember(single, rows(matched));
  eventOfRow = eventRows(matched);
  labels(single) = texts(eventOfRow(where));
  strs = [[""; labels], strs];
end
%=============================================================================
function texts = labelTexts(events)
  % Text of each event label, "<1 event>" for a missing or empty label.
  name = eventtable.variables(events).EventLabelsVariable;
  n = height(events);
  if isempty(name)
    texts = repmat("<1 event>", n, 1);
    return
  end
  values = subsref(events, struct('type', '.', 'subs', name));
  texts = strings(n, 1);
  for k = 1:n
    value = values(k, :);
    if iscell(value)
      value = value{1};
    end
    if isnumeric(value) && isnan(value)
      text = "";
    elseif isnumeric(value)
      text = string(num2str(value));
    else
      text = string(value);
    end
    if ismissing(text) || strlength(text) == 0
      text = "<1 event>";
    end
    texts(k) = text;
  end
end
%=============================================================================
