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
