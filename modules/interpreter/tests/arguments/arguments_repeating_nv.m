%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [xs, scale] = arguments_repeating_nv(x, opts)
  arguments (Repeating)
    x(1, 1) double
  end
  arguments
    opts.Scale(1, 1) double = 1
  end
  xs = x;
  scale = opts.Scale;
end
