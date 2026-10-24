%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefLocalFunctions
  properties (Dependent)
    Value
  end

  properties (Hidden)
    ValueStorage = 0
  end

  methods
    function obj = ClassdefLocalFunctions(value)
      if nargin == 0
        value = 0;
      end
      obj.ValueStorage = localNormalize(value);
    end

    function value = get.Value(obj)
      value = localScale(obj.ValueStorage);
    end

    function obj = set.Value(obj, value)
      obj.ValueStorage = localNormalize(value);
    end

    function value = combine(obj, value)
      value = obj.ValueStorage + localNormalize(value);
    end

    function value = privateLocalValue(obj)
      value = localPrivateValue(obj.ValueStorage);
    end

    function st = struct(obj)
      st = builtin('struct');
      st.Value = obj.Value;
      st.Combined = localNormalize(obj.ValueStorage);
    end
  end
end

function value = localNormalize(value)
  value = double(value) + 1;
end

function value = localScale(value)
  value = value * 2;
end

function value = localPrivateValue(value)
  value = value + classdefLocalFunctionsPrivateOffset();
end
