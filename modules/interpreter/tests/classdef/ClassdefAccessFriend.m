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
  end
end
