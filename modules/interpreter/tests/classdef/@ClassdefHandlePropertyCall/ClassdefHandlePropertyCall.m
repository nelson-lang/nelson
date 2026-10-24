%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A function-handle property whose name differs only in case from a method:
% obj.Inv(x) must read the property and call the handle, while obj.inv() calls
% the method.
classdef ClassdefHandlePropertyCall
  properties (GetAccess = 'public', SetAccess = 'public')
    Inv
  end
  methods
    function obj = ClassdefHandlePropertyCall()
      obj.Inv = @(x) x * 2;
    end
    function out = inv(obj) %#ok<MANU>
      out = 99;
    end
  end
end
