%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefEnumWithConstructor
  properties
    Code = 0
  end

  methods
    function obj = ClassdefEnumWithConstructor(code)
      if nargin > 0
        obj.Code = code;
      end
    end

    function r = isHigh(obj)
      r = obj == ClassdefEnumWithConstructor.High;
    end
  end

  enumeration
    Low(1)
    High(2)
  end
end
