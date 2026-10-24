%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef PackagedExternalMethod
  properties
    Value = 0
  end

  methods
    function obj = PackagedExternalMethod(value)
      if nargin > 0
        obj.Value = value;
      end
    end

    result = externalScale(obj, scale)
  end

  methods (Static)
    result = staticShift(value)
  end
end
