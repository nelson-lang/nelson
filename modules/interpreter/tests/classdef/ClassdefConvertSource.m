%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefConvertSource
  properties
    value = 0;
  end
  methods
    function o = ClassdefConvertSource(v)
      if nargin > 0
        o.value = v;
      end
    end
    function t = ClassdefConvertTarget(obj)
      % converter method: ClassdefConvertTarget(sourceObj) calls this
      t = ClassdefConvertTarget(obj.value * 10);
    end
  end
end
%=============================================================================
