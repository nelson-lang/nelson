%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function j = juliandate(t)
  narginchk(1, 1);
  if ~isa(t, 'datetime')
    error('Nelson:time:datetimeInputExpected', '%s expected.', 'Datetime input');
  end
  if isempty(t.TimeZone)
    j = datenum(t) + 1721058.5;
  else
    % a zoned datetime counts its UTC instant
    j = posixtime(t) ./ 86400 + 2440587.5;
  end
end
%=============================================================================
