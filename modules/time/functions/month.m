%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = month(t, option)
  narginchk(1, 2);
  if isa(t, 'datetime')
    m = t.Month;
  else
    v = datevec(t);
    m = reshape(v(:, 2), size(t));
  end
  if nargin == 2
    option = char(option);
    namesLong = ["January", "February", "March", "April", "May", "June", ...
      "July", "August", "September", "October", "November", "December"];
    namesShort = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", ...
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
    if strcmpi(option, 'name')
      varargout{1} = reshape(namesLong(m), size(m));
    elseif strcmpi(option, 'shortname')
      varargout{1} = reshape(namesShort(m), size(m));
    else
      error('Nelson:time:invalidMonthOption', 'Invalid %s.', 'month option');
    end
  else
    varargout{1} = m;
  end
end
%=============================================================================
