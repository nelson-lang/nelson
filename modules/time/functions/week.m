%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function w = week(t)
  narginchk(1, 1);
  serial = datenum(t);
  v = datevec(serial(:));
  firstDay = datenum(v(:, 1), 1, 1);
  % Weeks are counted as Sunday-starting calendar weeks; week 1 is the week
  % that contains January 1st, so the offset of that first weekday is included.
  dayOfYear = floor(serial(:)) - firstDay + 1;
  weekdayOfFirstDay = weekday(firstDay);
  w = floor((dayOfYear + weekdayOfFirstDay - 2) ./ 7) + 1;
  w = reshape(w, size(serial));
end
%=============================================================================
