%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [value, isterminal, direction] = odeTestTwoEvents(t, y)
  value = [y(1) - 0.25; y(1) - 0.5];
  isterminal = [0; 1];
  direction = [1; 1];
end
%=============================================================================
