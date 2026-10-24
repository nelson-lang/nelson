%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefDependentValue
  properties
    Base = 10
  end

  properties (Dependent)
    Twice
  end

  properties (Dependent, SetAccess = private)
    ReadOnlyTwice
  end

  methods
    function obj = ClassdefDependentValue(value)
      if nargin > 0
        obj.Base = value;
      end
    end

    function value = get.Twice(obj)
      value = obj.Base * 2;
    end

    function obj = set.Twice(obj, value)
      obj.Base = value / 2;
    end

    function value = get.ReadOnlyTwice(obj)
      value = obj.Base * 2;
    end

    function obj = set.ReadOnlyTwice(obj, value)
      obj.Base = value / 2;
    end

    function obj = setReadOnlyTwiceInside(obj, value)
      obj.ReadOnlyTwice = value;
    end
  end
end
