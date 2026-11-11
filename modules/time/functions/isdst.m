%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isdst(t)
  narginchk(1, 1);
  if ~isa(t, 'datetime')
    error('Nelson:time:datetimeInputExpected', '%s expected.', 'Datetime input');
  end
  timezoneDataPath = [modulepath('time'), '/resources/tzdata'];
  serials = datenum(t);
  tf = timezonedst(serials, t.TimeZone, timezoneDataPath);
  % first occurrences of ambiguous times (DST end) are daylight saving time
  first = datetime.foldOrFalse(t);
  if any(first(:))
    tf(first) = timezonedst(serials(first), t.TimeZone, timezoneDataPath, 'first');
  end
end
%=============================================================================
