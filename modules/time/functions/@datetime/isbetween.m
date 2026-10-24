%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isbetween(t, lowerBound, upperBound, intervalType)
  narginchk(3, 4);
  if nargin < 4
    intervalType = 'closed';
  end
  % datetime comparisons: instants of any time zone, never zoned with unzoned
  x = t;
  lo = lowerBound;
  hi = upperBound;
  intervalType = lower(char(intervalType));
  switch intervalType
    case {'closed', '[]'}
      tf = x >= lo & x <= hi;
    case {'open', '()'}
      tf = x > lo & x < hi;
    case {'openleft', '(]'}
      tf = x > lo & x <= hi;
    case {'openright', '[)'}
      tf = x >= lo & x < hi;
    otherwise
      error('Nelson:time:invalidIntervalType', 'Invalid %s.', 'interval type');
  end
end
%=============================================================================
