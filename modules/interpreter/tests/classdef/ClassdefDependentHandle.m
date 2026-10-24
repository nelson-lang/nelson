%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefDependentHandle < handle
  properties
    Base = 10
  end

  properties (Dependent)
    Twice
  end

  methods
    function obj = ClassdefDependentHandle(value)
      if nargin > 0
        obj.Base = value;
      end
    end

    function value = get.Twice(obj)
      value = obj.Base * 2;
    end

    function set.Twice(obj, value)
      obj.Base = value / 2;
    end
  end
end
