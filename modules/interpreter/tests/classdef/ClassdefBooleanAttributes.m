%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefBooleanAttributes
  properties (Hidden = false)
    VisibleValue = 1
  end

  properties (Constant = false)
    MutableValue = 2
  end

  properties (Hidden = 0, GetObservable = true, SetObservable = 1)
    ObservableValue = 3
  end

  methods (Hidden = false)
    function r = visibleMethod(obj)
      r = obj.VisibleValue + obj.MutableValue;
    end
  end

  methods (Static = false)
    function r = instanceOnly(obj)
      r = obj.MutableValue;
    end
  end
end
