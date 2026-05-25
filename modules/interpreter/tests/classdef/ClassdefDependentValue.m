classdef ClassdefDependentValue
  properties
    Base = 10
  end

  properties (Dependent)
    Twice
  end

  properties (Dependent, SetAccess = private)
    ReadOnlyTwice
  end

  methods
    function obj = ClassdefDependentValue(value)
      if nargin > 0
        obj.Base = value;
      end
    end

    function value = get.Twice(obj)
      value = obj.Base * 2;
    end

    function obj = set.Twice(obj, value)
      obj.Base = value / 2;
    end

    function value = get.ReadOnlyTwice(obj)
      value = obj.Base * 2;
    end

    function obj = set.ReadOnlyTwice(obj, value)
      obj.Base = value / 2;
    end

    function obj = setReadOnlyTwiceInside(obj, value)
      obj.ReadOnlyTwice = value;
    end
  end
end
