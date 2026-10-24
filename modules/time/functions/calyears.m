%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = calyears(x)
  narginchk(1, 1);
  if isa(x, 'calendarDuration')
    c = fix(calendarDuration.rawMonths(x) / 12);
  else
    c = calendarDuration(double(x), 0, 0);
  end
end
%=============================================================================
