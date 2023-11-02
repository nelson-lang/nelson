%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function color = getColorAndUpdateIndex(go)
    colorOrder = go.ColorOrder;
    if isempty (colorOrder)
      color = [0 0 0];
      return;
    end
    colorIndex = go.ColorOrderIndex;
    nbColors = size (colorOrder, 1);
    colorIndex = mod (colorIndex, nbColors);
    if (colorIndex == 0)
      colorIndex = nbColors;
    elseif (colorIndex < 0)
      colorIndex = 1;
    end
    color = colorOrder(colorIndex, :);
    if (colorIndex >= nbColors)
      colorIndex = colorIndex + 1;
      colorIndex = mod (colorIndex, nbColors);
      if (colorIndex == 0)
        colorIndex = 1;
      end
      lineStyleOrderIndex = go.LineStyleOrderIndex;
      go.LineStyleOrderIndex = lineStyleOrderIndex + 1;
    else
      colorIndex = colorIndex + 1;
    end
    go.ColorOrderIndex = colorIndex;
  end
  %=============================================================================
  