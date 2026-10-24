%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function P = quantile(A, varargin)
  % Quantiles of the serial-day values, NaT being removed as NaN.
  values = quantile(datenum(A), varargin{:});
  P = datetime.fromSerial(values, A.FormatValue, A.TimeZoneValue);
end
%=============================================================================
