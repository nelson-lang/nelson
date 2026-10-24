%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = lweekdate(weekdayNumber, yearNumber, monthNumber)
  narginchk(3, 3);
  last = datenum(yearNumber, monthNumber, eomday(yearNumber, monthNumber));
  lastWeekday = weekday(last);
  d = last - mod(lastWeekday - weekdayNumber, 7);
end
%=============================================================================
