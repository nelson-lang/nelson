%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Rebuild the static .m polar grid after a grid property changed by dot-set.
% Called from the C++ set path so that ThetaZeroLocation, ThetaDir, RColor,
% ThetaColor, RGrid and ThetaGrid stay in sync with the plotted data.
function __polar_refresh__(ax)
  if nargin < 1 || ~polarIsAxes(ax)
    return
  end
  polarSetStateAndRefresh(ax, polarGetState(ax));
end
%=============================================================================
