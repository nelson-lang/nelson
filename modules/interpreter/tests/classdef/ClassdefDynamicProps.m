%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefDynamicProps < dynamicprops & nelson.mixin.Copyable
  properties
    Base = 1
    LastEvent = ''
    LastProperty = ''
    StoredExtra = 0
  end
  methods
    function recordDynamicPropertyEvent(obj, eventData)
      obj.LastEvent = eventData.EventName;
      obj.LastProperty = eventData.PropertyName;
    end
    function value = readDynamicExtra(obj)
      value = obj.StoredExtra + 100;
    end
    function writeDynamicExtra(obj, value)
      obj.StoredExtra = value * 2;
    end
    function value = readDynamicExtraField(obj)
      value = obj.Extra;
    end
    function writeDynamicExtraField(obj, value)
      obj.Extra = value;
    end
  end
end
