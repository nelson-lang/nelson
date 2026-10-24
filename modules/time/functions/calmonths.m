%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = calmonths(x)
  narginchk(1, 1);
  if isa(x, 'calendarDuration')
    c = calendarDuration.rawMonths(x);
  else
    c = calendarDuration(0, double(x), 0);
  end
end
%=============================================================================
