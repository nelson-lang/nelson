%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefAbstractPropertyImpl < ClassdefAbstractPropertyBase
  properties
    RequiredValue = 43
  end

  properties (Access = protected)
    ProtectedValue(1, 1) double = 44
  end

  methods
    function value = protectedValue(obj)
      value = obj.ProtectedValue;
    end
  end
end
