%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [value, isterminal, direction] = odeTestFallingBallEvent(t, y)
  value = y(1);
  isterminal = 1;
  direction = -1;
end
%=============================================================================
