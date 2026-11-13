%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pinImageAxesLimits(ax, layout)
  % Fits X/Y limits with axis(ax, layout), then keeps them as manual limits.
  methods = {ax.XLimitMethod, ax.YLimitMethod, ax.ZLimitMethod};
  axis(ax, layout);
  xLimits = ax.XLim;
  yLimits = ax.YLim;
  ax.XLimitMethod = methods{1};
  ax.YLimitMethod = methods{2};
  ax.ZLimitMethod = methods{3};
  ax.XLim = xLimits;
  ax.YLim = yLimits;
end
%=============================================================================
