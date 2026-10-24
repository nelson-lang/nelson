%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [stop, y] = odeComplexCallbackState(t, y, index)
  asserts.isfalse(isreal(y));
  stop = true;
  y = y + 0.1 + 0.25i;
end
%=============================================================================
