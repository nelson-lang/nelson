%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A static method may be reached through an instance: obj.staticMethod(args).
classdef ClassdefStaticViaInstance
  properties
    v = 0
  end
  methods
    function obj = ClassdefStaticViaInstance(x)
      if nargin == 1
        obj.v = x;
      end
    end
    function r = addInstance(obj, y)
      r = obj.v + y;
    end
  end
  methods (Static = true)
    function r = combine(a, b)
      r = a * 10 + b;
    end
  end
end
%=============================================================================
