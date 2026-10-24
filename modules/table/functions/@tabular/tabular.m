%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Abstract) tabular
  methods
    function obj = tabular(varargin)
      if nargin ~= 0
        error('Nelson:table:invalidTabularConstruction', 'Invalid tabular construction.');
      end
    end
  end

  methods (Access = protected)
    value = checkPropertiesAssignment(obj, current, value)
    [data, names] = applyVariableProperties(obj, data, oldNames, props)
    validateNames(obj, names, message)
    tf = sameVariableProperties(obj, a, b)
    names = toCellstrRow(obj, names)
    names = checkedVariableNames(obj, names)
    names = checkedRowNames(obj, names)
    names = checkedDimensionNames(obj, names)
    checkDimensionVariableNames(obj, props)
    checkReservedVariableNames(obj, names)
  end
end
%=============================================================================
