%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [a, b, c, k] = scalar_range_loop_values(first, increment, last, a, b)
  c = a;
  k = 42;
  for k = first:increment:last
    c = a + b;
    a = b;
    b = c;
  end
end
