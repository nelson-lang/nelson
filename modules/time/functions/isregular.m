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
    d = diff(serial(:));
    tf = all(abs(d - d(1)) < 1e-10);
    return
  end
  unit = lower(char(unit));
  switch unit
    case {'years', 'year'}
      d = diff(year(t(:)));
    case {'quarters', 'quarter'}
      d = diff(year(t(:)) .* 4 + quarter(t(:)));
    case {'months', 'month'}
      d = diff(year(t(:)) .* 12 + month(t(:)));
    case {'weeks', 'week'}
      d = diff(floor(serial(:) ./ 7));
    case {'days', 'day'}
      d = diff(floor(serial(:)));
    otherwise
      d = diff(serial(:));
  end
  tf = all(d == d(1));
end
%=============================================================================
