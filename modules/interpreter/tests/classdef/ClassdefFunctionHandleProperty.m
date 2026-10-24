%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefFunctionHandleProperty
  properties
    Transform = @(x) x + 1
  end

  methods
    function obj = ClassdefFunctionHandleProperty(fh)
      if nargin > 0
        obj.Transform = fh;
      end
    end

    function value = apply(obj, input)
      value = obj.Transform(input);
    end
  end
end
