%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefAbstractPropertyVariantsImpl < ClassdefAbstractPropertyVariants
  properties (Constant)
    ConstantValue = 10
  end

  properties (Dependent)
    DependentValue
  end

  properties (Hidden)
    HiddenValue = 30
  end

  methods
    function value = get.DependentValue(obj)
      value = 20;
    end
  end
end
