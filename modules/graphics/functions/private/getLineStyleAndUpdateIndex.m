%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function lineStyle = getLineStyleAndUpdateIndex(go)
  lineStyleOrder = go.LineStyleOrder;
  if isempty (lineStyleOrder)
    lineStyle = '-';
    return;
  end
  if ischar(lineStyleOrder)
    lineStyleOrder = cellstr(lineStyleOrder);
  end
  LineStyleOrderIndex = go.LineStyleOrderIndex;
  numberOfStyles = size(lineStyleOrder, 1);
  LineStyleOrderIndex = mod (LineStyleOrderIndex, numberOfStyles);
  if (LineStyleOrderIndex == 0)
    LineStyleOrderIndex = numberOfStyles;
  elseif (LineStyleOrderIndex < 0)
    LineStyleOrderIndex = 1;
  end
  lineStyle = lineStyleOrder{LineStyleOrderIndex};
end
%=============================================================================
