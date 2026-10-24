%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = posixtime(t)
  narginchk(1, 1);
  if ~isa(t, 'datetime')
    error('Nelson:time:datetimeInputExpected', '%s expected.', 'Datetime input');
  end
  % Use the millisecond-accurate instant so seconds past the epoch are exact.
  % For dates far from the serial-day origin, folding the time of day into the
  % large day count loses microseconds; the datetime keeps the precise value.
  p = datetime.posixSecondsOf(t);
end
%=============================================================================
