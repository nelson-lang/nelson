%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = arguments_edge_multiline_default(a, b, c)
  arguments
    a double = [1 2
    3 4]
    b double = max(10, ...
                   20)
    c double = [100
    200]
  end
  y = sum(a(:)) + b + sum(c);
end
%=============================================================================
