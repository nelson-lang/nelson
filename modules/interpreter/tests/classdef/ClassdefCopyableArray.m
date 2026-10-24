%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefCopyableArray < nelson.mixin.Copyable
  properties
    Value = 0
    Ref = []
  end
  methods
    function obj = ClassdefCopyableArray(value)
      if nargin >= 1
        obj.Value = value;
      end
    end
  end
end
%=============================================================================
