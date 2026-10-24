%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function args = graphicsSelectTableSeriesArgs(tableArgs, k, nSeries)
  args = {};
  if isempty(tableArgs)
    return
  end
  if nSeries == 1
    args = tableArgs{1};
  else
    args = tableArgs{k};
  end
end
%=============================================================================
