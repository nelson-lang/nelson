%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefStoredPropertySetter
  properties
    Value = []
  end

  methods
    function obj = ClassdefStoredPropertySetter(value)
      if nargin > 0
        obj = assignStoredProperty(obj, value);
      end
    end

    function obj = set.Value(obj, value)
      if ~isfinite(value)
        error('Stored value must be finite.');
      end
      obj.Value = value * 2;
    end
  end
end

function obj = assignStoredProperty(obj, value)
  obj.Value = value;
end
