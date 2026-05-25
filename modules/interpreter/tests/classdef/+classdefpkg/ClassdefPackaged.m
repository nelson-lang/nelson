classdef ClassdefPackaged
  properties
    Value = 3
  end

  properties (Constant)
    ConstantValue = 12
  end

  methods
    function obj = ClassdefPackaged(value)
      if nargin > 0
        obj.Value = value;
      end
    end

    function r = triple(obj)
      r = obj.Value * 3;
    end
  end

  methods (Static)
    function obj = origin()
      obj = classdefpkg.ClassdefPackaged(0);
    end
  end
end
