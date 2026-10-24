%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rowTimes = timetableGrowRowTimes(rowTimes, newHeight, newTimes)
  % Row times of a timetable grown to newHeight rows by an assignment: the
  % times given in the row subscript (newTimes), then missing times (NaN
  % for durations, NaT for datetimes).
  nRows = timeCount(rowTimes);
  if newHeight <= nRows
    return
  end
  if nargin > 2 && timeCount(newTimes) > 0
    rowTimes = [rowTimes; newTimes];
    nRows = nRows + timeCount(newTimes);
  end
  if newHeight > nRows
    rowTimes(nRows + 1:newHeight, 1) = missing;
  end
end
%=============================================================================
function n = timeCount(value)
  if isa(value, 'datetime')
    n = numel(datenum(value));
  else
    n = numel(seconds(value));
  end
end
%=============================================================================
