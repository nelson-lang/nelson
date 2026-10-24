%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ChainLeaf < handle
  methods
    function r = Scale(obj, x)
      r = 10 * x;
    end
    function r = add(obj, x)
      r = x + 1;
    end
  end
end
