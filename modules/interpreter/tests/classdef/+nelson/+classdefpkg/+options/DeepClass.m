%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef DeepClass
  properties
    Value = 4
    Child = []
    Callback = []
    Objective = []
  end

  methods
    function obj = DeepClass(value, child, varargin)
      if nargin > 0
        obj.Value = value;
      end
      if nargin > 1
        obj.Child = nelson.classdefpkg.options.DeepClass.identity(child);
      end
      if mod(length(varargin), 2) ~= 0
        error('Name-value arguments expected.');
      end
      for k = 1:2:length(varargin)
        name = lower(char(varargin{k}));
        value = varargin{k + 1};
        switch name
          case 'child'
            obj.Child = nelson.classdefpkg.options.DeepClass.identity(value);
          case 'callback'
            obj.Callback = value;
          case 'objective'
            objective = nelson.classdefpkg.options.DeepClass.identity(value);
            obj.Objective = objective;
        end
      end
    end

    function r = scale(obj)
      r = obj.Value * nelsonClassdefPrivateScale();
    end

    function r = evaluateWithVector(obj, x)
      r = double((x - obj.Value) .^ 2);
    end

    function r = evaluateChildWithVector(obj, x)
      r = double(obj.Child.evaluateWithVector(x));
    end

    function r = callCallback(obj, x)
      callback = obj.Callback;
      r = callback(x);
    end
  end

  methods (Static)
    function obj = origin()
      obj = DeepClass(0);
    end

    function r = staticValue(value)
      r = value * 6;
    end

    function value = identity(value)
    end
  end
end
