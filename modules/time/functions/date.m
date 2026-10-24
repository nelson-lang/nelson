%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function t = date()
  currentClock = clock();
  monthNames = {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', ...
                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'};
  dayText = sprintf('%02.0f', currentClock(3));
  yearText = sprintf('%.0f', currentClock(1));
  t = [dayText '-' monthNames{currentClock(2)} '-' yearText];
end
%=============================================================================

