%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefMultiOutput
  properties
    Value
  end

  methods
    function obj = ClassdefMultiOutput(value)
      if nargin == 0
        value = 1;
      end
      [a, b] = splitValue(obj, value);
      obj.Value = a + b;
    end
  end

  methods (Access = private)
    function [a, b] = splitValue(obj, value)
      a = value;
      b = value;
    end
  end
end
