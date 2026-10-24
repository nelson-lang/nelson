%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefPropertyAccessBase < handle
  properties (GetAccess = public, SetAccess = protected)
    Counter = 1
  end

  properties (Access = private)
    Secret = 42
  end

  properties (Access = protected)
    Guarded = 7
  end

  properties (Dependent)
    Twice
  end

  properties (SetAccess = immutable)
    Id
  end

  properties (Constant)
    Limit = 10
  end

  methods
    function obj = ClassdefPropertyAccessBase()
      obj.Id = 5;
    end

    function value = get.Twice(obj)
      value = 2 * obj.Counter;
    end

    function set.Twice(obj, value)
      obj.Counter = value / 2;
    end

    function value = readSecret(obj)
      value = obj.Secret;
    end

    function writeSecret(obj, value)
      obj.Secret = value;
    end

    function bump(obj)
      obj.Counter = obj.Counter + 1;
    end
  end

  methods (Static)
    function value = callHelper(obj)
      value = ClassdefPropertyAccessBase.helper(obj);
    end
  end

  methods (Static, Access = private)
    function value = helper(obj)
      value = obj.Secret;
    end
  end
end
