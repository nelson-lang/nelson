%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = eomday(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  y = varargin{1};
  m = varargin{2};
  
  if ~isnumeric(m) || any(m(:) < 1) || any(12 < m(:)) || any(fix(m(:)) ~= m(:))
    error('Nelson:eomday:NotAMonthNumber', _('M must be a month number from 1 to 12.'));
  end
  
  if ~isnumeric(y) || any(fix(y(:)) ~= y(:))
    error('Nelson:eomday:NotAYearNumber', _('Y must be an integer year number.'));
  end
  
  numberDaysInMonth = [31 28 31 30 31 30 31 31 30 31 30 31]';
  d = y - m;
  d(:) = numberDaysInMonth(m);
  
  is29 = (m == 2) & ((rem(y,4) == 0 & rem(y, 100) ~= 0) | rem(y, 400) == 0);
  d(is29) = 29;
end