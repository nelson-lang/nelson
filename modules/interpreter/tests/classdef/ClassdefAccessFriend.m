%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefAccessFriend
  methods
    function r = readFriendMethod(obj, target)
      r = target.friendOnly();
    end

    function r = readFriendDirectMethod(obj, target)
      r = target.friendOnlyDirect();
    end

    function r = readFriendProperty(obj, target)
      r = target.FriendValue;
    end

    function target = setFriendProperty(obj, target, value)
      target.FriendValue = value;
    end

    function r = readListProperty(obj, target)
      r = target.SharedValue;
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
  end
end
