%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = timetableMathBinary(A, B, fun)
  if timetableHasEventtable({A, B})
    R = eventtable.applyTimetableFunction(fun, A, B);
  elseif istimetable(A) && istimetable(B)
    B = alignTimetableRows(A, B, func2str(fun));
    T = table.applyBinary(timetable2table(A, 'ConvertRowTimes', false), ...
      timetable2table(B, 'ConvertRowTimes', false), fun);
    R = timetable.fromTableAndTimes(T, A.Properties.RowTimes, A.Properties);
  elseif istimetable(A)
    if istable(B) && ~isempty(B.Properties.RowNames)
      error('Nelson:timetable:invalidCombination', 'Table input cannot have row names when combined with a timetable.');
    end
    T = table.applyBinary(timetable2table(A, 'ConvertRowTimes', false), B, fun);
    R = timetable.fromTableAndTimes(T, A.Properties.RowTimes, A.Properties);
  elseif istimetable(B)
    if istable(A) && ~isempty(A.Properties.RowNames)
      error('Nelson:timetable:invalidCombination', 'Table input cannot have row names when combined with a timetable.');
    end
    T = table.applyBinary(A, timetable2table(B, 'ConvertRowTimes', false), fun);
    R = timetable.fromTableAndTimes(T, B.Properties.RowTimes, B.Properties);
  else
    R = fun(A, B);
  end
end
%=============================================================================
function B = alignTimetableRows(A, B, functionName)
  if height(B) == 1 && height(A) ~= 1
    return
  end
  if height(A) ~= height(B)
    error('Nelson:timetable:dimagree', '%s must have the same height, or one input must have one row.', 'Timetables');
  end
  order = zeros(height(A), 1);
  for k = 1:height(A)
    idx = find(B.Properties.RowTimes == A.Properties.RowTimes(k), 1);
    if isempty(idx)
      error(['Nelson:timetable:', functionName, ':RowTimesMismatch'], sprintf(_('%s must match.'), 'Timetable row times'));
    end
    order(k) = idx;
  end
  B = B(order, :);
end
%=============================================================================
