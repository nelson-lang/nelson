%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = day(t, option)
  narginchk(1, 2);
  serial = datenum(t);
  v = datevec(serial(:));
  if nargin == 1 || strcmpi(char(option), 'dayofmonth')
    d = reshape(v(:, 3), size(serial));
  elseif strcmpi(char(option), 'dayofyear')
    d = floor(serial(:)) - datenum(v(:, 1), 1, 0);
    d = reshape(d, size(serial));
  elseif strcmpi(char(option), 'name') || strcmpi(char(option), 'shortname')
    if strcmpi(char(option), 'name')
      format = 'long';
    else
      format = 'short';
    end
    [~, names] = weekday(serial, format);
    d = string(cellstr(names));
  else
    error('Nelson:time:invalidDayOption', 'Invalid %s.', 'day option');
  end
  varargout{1} = d;
end
%=============================================================================
