classdef ClassdefEnumWithConstructor
  properties
    Code = 0
  end

  methods
    function obj = ClassdefEnumWithConstructor(code)
      if nargin > 0
        obj.Code = code;
      end
    end

    function r = isHigh(obj)
      r = obj == ClassdefEnumWithConstructor.High;
    end
  end

  enumeration
    Low(1)
    High(2)
  end
end
