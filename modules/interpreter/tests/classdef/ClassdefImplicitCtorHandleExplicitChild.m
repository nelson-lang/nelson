%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefImplicitCtorHandleExplicitChild < ClassdefImplicitCtorHandleBase
  properties
    w
  end
  methods
    function self = ClassdefImplicitCtorHandleExplicitChild(value)
      self.w = [self.v, '-', value];
    end
  end
end
