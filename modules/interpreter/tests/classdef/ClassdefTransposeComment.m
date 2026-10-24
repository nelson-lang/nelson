%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefTransposeComment
  properties
    A = [1 2 3]' % transpose then comment
    S = 'it''s here' % escaped quote then comment
    N = 5
  end
end
%=============================================================================
