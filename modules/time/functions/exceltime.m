%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function e = exceltime(t)
  narginchk(1, 1);
  if ~isa(t, 'datetime')
    error('Nelson:time:datetimeInputExpected', '%s expected.', 'Datetime input');
  end
  dn = datenum(t);
  % Excel serial dates count from 1899-12-30. Excel also incorrectly treats
  % 1900 as a leap year (a phantom 1900-02-29), so dates on or after
  % 1900-03-01 include that extra day and use the 1899-12-30 origin, while
  % earlier dates use 1899-12-31 to skip the non-existent leap day.
  e = dn - datenum(1899, 12, 30);
  before = dn < datenum(1900, 3, 1);
  e(before) = dn(before) - datenum(1899, 12, 31);
end
%=============================================================================
