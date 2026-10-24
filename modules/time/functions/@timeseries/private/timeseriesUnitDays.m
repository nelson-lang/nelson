%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function scale = timeseriesUnitDays(units)
  switch lower(units)
    case {'second', 'seconds', 'sec', 'secs'}
      scale = 1 / 86400;
    case {'minute', 'minutes', 'min', 'mins'}
      scale = 1 / 1440;
    case {'hour', 'hours', 'hr', 'hrs'}
      scale = 1 / 24;
    case {'day', 'days'}
      scale = 1;
    otherwise
      scale = 1 / 86400;
  end
end
%=============================================================================
