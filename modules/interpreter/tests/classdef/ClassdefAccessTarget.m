%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefAccessTarget
  properties (Access = ?ClassdefAccessFriend)
    FriendValue = 17
  end

  methods
    function obj = ClassdefAccessTarget(value)
      if nargin > 0
        obj.FriendValue = value;
      end
    end

    function r = readFromDefiningClass(obj)
      r = obj.friendOnly();
    end
  end

  methods (Access = {?ClassdefAccessFriend})
    function r = friendOnly(obj)
      r = obj.FriendValue;
    end
  end

  methods (Access = ?ClassdefAccessFriend)
    function r = friendOnlyDirect(obj)
      r = obj.FriendValue + 1;
    end
  end
end
