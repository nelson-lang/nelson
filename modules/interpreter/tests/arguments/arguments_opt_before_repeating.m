%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [a, xs] = arguments_opt_before_repeating(a, x)
  arguments
    a(1, 1) double = 42
  end
  arguments (Repeating)
    x(1, 1) double
  end
  xs = x;
end
