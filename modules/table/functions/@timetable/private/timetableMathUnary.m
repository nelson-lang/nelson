%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = timetableMathUnary(A, fun, functionName)
  if isa(A, 'eventtable')
    R = eventtable.applyTimetableFunction(@(TT) timetableMathUnary(TT, fun, functionName), A);
    return
  end
  T = table.applyUnary(timetable2table(A, 'ConvertRowTimes', false), fun, functionName);
  R = timetable.fromTableAndTimes(T, A.Properties.RowTimes, A.Properties);
end
%=============================================================================
