%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef griddedInterpolant
  properties (Dependent)
    GridVectors
    Values
    Method
    ExtrapolationMethod
  end

  properties (Hidden)
    GridVectorsValue = {}
    ValuesValue = []
    MethodValue = 'linear'
    ExtrapolationMethodValue = 'linear'
  end

  methods
    function obj = griddedInterpolant(varargin)
      if nargin == 0
        return
      end
      [gridVectors, values, method, extrap] = griddedInterpolantParse(varargin{:});
      obj.GridVectorsValue = gridVectors;
      obj.ValuesValue = values;
      obj.MethodValue = method;
      obj.ExtrapolationMethodValue = extrap;
    end

    function value = get.GridVectors(obj)
      value = obj.GridVectorsValue;
    end

    function obj = set.GridVectors(obj, value)
      obj.GridVectorsValue = griddedInterpolantCheckGridVectors(value);
    end

    function value = get.Values(obj)
      value = obj.ValuesValue;
    end

    function obj = set.Values(obj, value)
      obj.ValuesValue = griddedInterpolantCheckValues(value);
    end

    function value = get.Method(obj)
      value = obj.MethodValue;
    end

    function obj = set.Method(obj, value)
      obj.MethodValue = griddedInterpolantValidateMethod(value);
    end

    function value = get.ExtrapolationMethod(obj)
      value = obj.ExtrapolationMethodValue;
    end

    function obj = set.ExtrapolationMethod(obj, value)
      obj.ExtrapolationMethodValue = griddedInterpolantValidateExtrap(value);
    end

    vq = evaluate(obj, varargin)
    varargout = subsref(obj, s)
    disp(obj)
    display(obj)
  end
end
%=============================================================================
