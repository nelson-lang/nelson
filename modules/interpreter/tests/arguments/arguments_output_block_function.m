%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = arguments_output_block_function(A, B, C)
  arguments (Input)
    A (1,1) string
    B (1,:) double
    C (2,2) cell
  end

  arguments (Output)
    out (1,:) double
  end

  out = B;
end
%=============================================================================
