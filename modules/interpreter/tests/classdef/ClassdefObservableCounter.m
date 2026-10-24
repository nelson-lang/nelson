%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefObservableCounter < handle
  properties (GetObservable, SetObservable)
    Count = 0
  end

  properties (AbortSet, SetObservable)
    StableCount = 0
  end

  properties
    LastEvent = ''
    LastProperty = ''
    EventCount = 0
  end

  methods
    function obj = ClassdefObservableCounter(value)
      if nargin > 0
        obj.Count = value;
      end
    end

    function recordPropertyEvent(obj, eventData)
      obj.LastEvent = eventData.EventName;
      obj.LastProperty = eventData.PropertyName;
      obj.EventCount = obj.EventCount + 1;
    end
  end
end
