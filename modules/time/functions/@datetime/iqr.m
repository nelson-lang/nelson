%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [r, q] = iqr(A, varargin)
  % Interquartile range of the serial-day values (NaT removed as NaN):
  % the range is a duration, the quartiles are datetime values.
  if nargout > 1
    [days, q] = iqr(datenum(A), varargin{:});
    q = datetime.fromSerial(q, A.FormatValue, A.TimeZoneValue);
  else
    days = iqr(datenum(A), varargin{:});
  end
  r = duration.fromSeconds(days * 86400);
end
%=============================================================================
