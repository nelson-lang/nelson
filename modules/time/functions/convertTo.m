%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = convertTo(t, kind, varargin)
  narginchk(2, 6);
  if ~isa(t, 'datetime')
    error('Nelson:time:datetimeInputExpected', '%s expected.', 'Datetime input');
  end
  kind = lower(char(kind));
  switch kind
    case 'datenum'
      value = datenum(t);
    case 'posixtime'
      value = posixtime(t);
    case 'juliandate'
      value = juliandate(t);
    case {'excel', 'exceltime'}
      value = exceltime(t);
    case 'yyyymmdd'
      value = yyyymmdd(t);
    otherwise
      error('Nelson:time:unsupportedDatetimeConversion', 'Unsupported %s.', 'datetime conversion');
  end
end
%=============================================================================
