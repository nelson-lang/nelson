%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isregular(t, unit)
  narginchk(1, 2);
  serial = datenum(t);
  if numel(serial) <= 2
    tf = true;
    return
  end
  if nargin < 2
    tf = isRegularSteps(diff(serial(:)));
    return
  end
  tf = isRegularIn(t(:), serial(:), lower(char(unit)));
end
%=============================================================================
function tf = isRegularIn(t, serial, unit)
  % constant steps in a calendar unit, or month (quarter) ends
  if any(strcmp(unit, {'endofmonth', 'endofquarter'}))
    tf = isRegularAtEnds(t, unit);
    return
  end
  d = unitSteps(t, serial, unit);
  tf = all(d == d(1));
end
%=============================================================================
function tf = isRegularSteps(d)
  tf = all(abs(d - d(1)) < 1e-10);
end
%=============================================================================
function d = unitSteps(t, serial, unit)
  % steps between consecutive dates counted in unit
  switch unit
    case {'years', 'year'}
      d = diff(year(t));
    case {'quarters', 'quarter'}
      d = diff(year(t) .* 4 + quarter(t));
    case {'months', 'month'}
      d = diff(year(t) .* 12 + month(t));
    case {'weeks', 'week'}
      d = diff(floor(serial ./ 7));
    case {'days', 'day'}
      d = diff(floor(serial));
    otherwise
      d = diff(serial);
  end
end
%=============================================================================
function tf = isRegularAtEnds(t, unit)
  % every date at a month (quarter) end, a constant number of months
  % (quarters) apart
  v = datevec(t);
  atEnd = v(:, 3) == eomday(v(:, 1), v(:, 2));
  step = 1;
  if strcmp(unit, 'endofquarter')
    atEnd = atEnd & mod(v(:, 2), 3) == 0;
    step = 3;
  end
  d = diff(v(:, 1) .* 12 + v(:, 2)) ./ step;
  tf = all(atEnd) && all(d == d(1));
end
%=============================================================================
