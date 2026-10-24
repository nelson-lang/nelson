%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function time = timeseriesNormalizeTime(value)
  if isempty(value)
    time = zeros(0, 1);
  elseif isa(value, 'datetime')
    time = datenum(value);
    time = time(:);
  elseif isa(value, 'duration')
    time = double(value);
    time = time(:);
  elseif ischar(value) || isstring(value) || iscellstr(value)
    time = datenum(value);
    time = time(:);
  elseif isnumeric(value)
    time = double(value(:));
  else
    error('Nelson:time:timeValuesNumericDatetimeDuration', 'Time values must be numeric, datetime, duration, or text dates.');
  end
end
%=============================================================================
