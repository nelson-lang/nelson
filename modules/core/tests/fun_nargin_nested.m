%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = fun_nargin_nested()
  h = @nestedThree;
  function [x, y] = nestedThree(a, b, c)
    x = a + b + c;
    y = a - b;
  end
end
%=============================================================================
