%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefConstantLocalFunction
  properties (Constant = true)
    FromLocal = makeConstantValue()
    Chained = makeChainedValue()
    Literal = 5
    Derived = 2 * ClassdefConstantLocalFunction.Literal
  end

  methods
    function value = fromMethod(~)
      value = makeConstantValue();
    end
  end
end

function v = makeConstantValue()
  v = 99;
end

function v = makeChainedValue()
  v = makeConstantValue() + 1;
end
