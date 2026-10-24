%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefDependentValidatedHandle < handle
  properties
    StoredValue = single(0)
    StoredRowValue = single([0 0])
    LastAssignedClass = ''
    LastAssignedRowClass = ''
  end

  properties (Dependent)
    ScaledValue(1, 1) single
    ScaledRowValue(1, 2) single
  end

  methods
    function value = get.ScaledValue(obj)
      value = obj.StoredValue;
    end

    function set.ScaledValue(obj, value)
      obj.LastAssignedClass = class(value);
      obj.StoredValue = value * single(2);
    end

    function value = get.ScaledRowValue(obj)
      value = obj.StoredRowValue;
    end

    function set.ScaledRowValue(obj, value)
      obj.LastAssignedRowClass = class(value);
      obj.StoredRowValue = value * single(2);
    end
  end
end
