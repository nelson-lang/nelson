%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = arguments_edge_transpose_default(a)
  arguments
    a double = [1 2 3]' % transpose then comment
  end
  y = sum(a);
end
%=============================================================================
