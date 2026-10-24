%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefPackaged
  properties
    Value = 3
  end

  properties (Constant)
    ConstantValue = 12
  end

  methods
    function obj = ClassdefPackaged(value)
      if nargin > 0
        obj.Value = value;
      end
    end

    function r = triple(obj)
      r = obj.Value * 3;
    end
  end

  methods (Static)
    function obj = origin()
      obj = classdefpkg.ClassdefPackaged(0);
    end
  end
end
