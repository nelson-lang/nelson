%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = odeHasCrossing(v0, v1, direction)
  tf = (v1 == 0) || (v0 ~= 0 && sign(v0) ~= sign(v1));
  if ~tf || direction == 0
    return
  end
  slope = v1 - v0;
  tf = (direction > 0 && slope > 0) || (direction < 0 && slope < 0);
end
%=============================================================================
