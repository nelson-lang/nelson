%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [tf, tfRow] = withinrange(TT, timeSpec)
  narginchk(2, 2);
  [first, last] = boundsLocal(timeSpec);
  times = TT.Properties.RowTimes;
  tfRow = times >= first & times <= last;
  tf = all(tfRow);
end
%=============================================================================
function [first, last] = boundsLocal(timeSpec)
  if timeLengthLocal(timeSpec) ~= 2
    error('Nelson:timetable:invalidValue', 'Time range must contain two values.');
  end
  first = timeSpec(1);
  last = timeSpec(2);
end
%=============================================================================
function n = timeLengthLocal(value)
  if isa(value, 'datetime')
    n = numel(datenum(value));
  elseif isa(value, 'duration')
    n = numel(seconds(value));
  else
    n = length(value);
  end
end
%=============================================================================
