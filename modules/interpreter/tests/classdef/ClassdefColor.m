classdef ClassdefColor
  enumeration
    Red
    Blue
  end

  methods
    function r = isRed(obj)
      r = obj == ClassdefColor.Red;
    end
  end
end
