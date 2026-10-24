%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function b = datetimeAlignOperand(a, b)
  % Second operand of a two-datetime operation expressed in the time zone of
  % the first one, so their serial values compare as instants.
  if isa(a, 'datetime') && isa(b, 'datetime')
    b = datetimeAlignZone(b, a.TimeZoneValue);
  end
end
%=============================================================================
