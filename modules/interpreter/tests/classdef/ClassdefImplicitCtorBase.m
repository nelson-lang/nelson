%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefImplicitCtorBase
  properties
    v
    calls = 0
  end
  methods
    function obj = ClassdefImplicitCtorBase()
      obj.v = 42;
      obj.calls = obj.calls + 1;
    end
  end
end
