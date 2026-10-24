%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [xs, ys] = arguments_repeating_pairs(x, y)
  arguments (Repeating)
    x(1, 1) double
    y(1, :) char
  end
  xs = x;
  ys = y;
end
