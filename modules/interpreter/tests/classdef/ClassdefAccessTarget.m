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
