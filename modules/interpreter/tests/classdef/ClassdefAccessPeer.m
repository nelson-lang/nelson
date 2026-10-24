%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefAccessPeer
  methods
    function r = readListProperty(obj, target)
      r = target.SharedValue;
    end

    function target = setListProperty(obj, target, value)
      target.SharedValue = value;
    end

    function r = readListMethod(obj, target)
      r = target.sharedMethod();
    end

    function r = readListConstant(obj)
      r = ClassdefAccessListTarget.SharedConstant;
    end

    function r = readListStatic(obj)
      r = ClassdefAccessListTarget.sharedStatic();
    end

    function listener = listenListEvent(obj, target)
      listener = addlistener(target, 'SharedEvent', @(src, eventData) target.recordEvent(eventData.EventName));
    end

    function triggerListEvent(obj, target)
      notify(target, 'SharedEvent');
    end
  end
end
