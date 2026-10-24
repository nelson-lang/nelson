%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = nweekdate(n, weekdayNumber, yearNumber, monthNumber)
  narginchk(4, 4);
  first = datenum(yearNumber, monthNumber, 1);
  firstWeekday = weekday(first);
  offset = mod(weekdayNumber - firstWeekday, 7);
  d = first + offset + (n - 1) .* 7;
  invalid = month(datetime(d, 'ConvertFrom', 'datenum')) ~= monthNumber;
  d(invalid) = NaN;
end
%=============================================================================
