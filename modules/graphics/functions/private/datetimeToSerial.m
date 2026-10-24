%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Converts a datetime array to its serial date numbers so it can flow through
% the numeric graphics data pipeline. Returns the original value untouched for
% any non datetime input, together with a flag telling whether a conversion
% happened (used by callers to switch the matching axis into date mode).
function [value, isDate] = datetimeToSerial(value)
  isDate = isa(value, 'datetime');
  if isDate
    value = datenum(value);
  end
end
%=============================================================================
