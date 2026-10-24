%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = colon(a, b, c)
  narginchk(2, 3);
  if nargin == 2
    first = a;
    step = days(1);
    last = b;
  else
    first = a;
    step = b;
    last = c;
  end
  if ~isa(first, 'datetime') || ~isa(step, 'duration') || ~isa(last, 'datetime')
    error('Nelson:time:datetimeColonOperandsExpected', '%s colon operands expected.', 'Datetime');
  end
  serial = datenum(first):seconds(step) ./ 86400:datenum(last);
  out = datetime(serial, 'ConvertFrom', 'datenum', 'Format', first.Format, 'TimeZone', first.TimeZone);
end
%=============================================================================
