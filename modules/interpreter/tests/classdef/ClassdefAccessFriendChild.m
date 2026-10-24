%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefAccessFriendChild < ClassdefAccessFriend
  methods
    function r = readAsChild(obj, target)
      r = target.friendOnly();
    end

    function r = readDirectAsChild(obj, target)
      r = target.friendOnlyDirect();
    end

    function r = readPropertyAsChild(obj, target)
      r = target.FriendValue;
    end

    function target = setPropertyAsChild(obj, target, value)
      target.FriendValue = value;
    end
  end
end
