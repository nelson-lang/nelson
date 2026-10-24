%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef NcBase
  properties
    Value = 0
  end
  methods
    function object = NcBase(value)
      object.Value = value;
    end
    function result = inheritedValue(object)
      result = nc_class_scale(object.Value);
    end
  end
end
