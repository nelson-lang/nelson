%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefContinuationCommentDefault
  properties
    A = 1 + ... trailing comment after the continuation
        2
    B(1, ...
       2) double = [10 ... keep going
                    20]
  end
end
%=============================================================================
