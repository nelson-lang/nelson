%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = graphics_comet3_any_full_trace(handles, refX, refY, refZ)
  r = false;
  for k = 1:numel(handles)
    [x, y, z] = getpoints(handles(k));
    if isequal(x, refX) && isequal(y, refY) && isequal(z, refZ)
      r = true;
    end
  end
end
%=============================================================================
