%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [value, isterminal, direction] = odeTestBadEventValue(t, y)
  value = Inf;
  isterminal = 1;
  direction = 0;
end
%=============================================================================
