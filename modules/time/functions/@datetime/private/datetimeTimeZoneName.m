%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function name = datetimeTimeZoneName(value)
  % Canonical name of a TimeZone value: text naming a time zone ('local' is
  % the system time zone) or a duration giving a fixed UTC offset.
  if isa(value, 'duration') && isscalar(value) && ~isnan(value)
    total = round(minutes(value));
    signs = '+-';
    name = sprintf('%c%02d:%02d', signs(1 + (total < 0)), floor(abs(total) / 60), mod(abs(total), 60));
  elseif ischar(value) && (isrow(value) || isempty(value)) || (isstring(value) && isscalar(value) && ~ismissing(value))
    name = char(value);
  else
    error('Nelson:datetime:InvalidTimeZone', 'A time zone must be specified as a string scalar, non-NaN duration scalar, or character vector.');
  end
  if ~isempty(name)
    name = timezonename(name, [modulepath('time'), '/resources/tzdata']);
  end
end
%=============================================================================
