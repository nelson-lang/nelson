%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefSubsindexValue
  % subsindex returns the stored value as it is (zero-based positions)
  properties
    r = 0
  end
  methods
    function obj = ClassdefSubsindexValue(x)
      obj.r = x;
    end
    function idx = subsindex(obj)
      idx = obj.r;
    end
  end
end
%=============================================================================
