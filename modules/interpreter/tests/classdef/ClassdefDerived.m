classdef ClassdefDerived < ClassdefBase
  properties
    DerivedValue = 20
  end

  methods
    function obj = ClassdefDerived(value)
      if nargin > 0
        obj.DerivedValue = value;
      end
    end

    function r = derivedMethod(obj)
      r = obj.BaseValue + obj.DerivedValue;
    end

    function r = callProtected(obj)
      r = protectedMethod(obj) + obj.protectedMethod();
    end

    function r = readBaseProtectedProperty(obj)
      r = obj.BaseProtectedValue;
    end

    function r = cannotCallBasePrivate(obj)
      try
        privateMethod(obj);
        r = 'allowed';
      catch
        r = 'blocked';
      end
    end

    function r = cannotReadBasePrivateProperty(obj)
      try
        obj.BasePrivateValue;
        r = 'allowed';
      catch
        r = 'blocked';
      end
    end
  end

  methods (Static)
    function r = callProtectedStatic()
      r = ClassdefBase.protectedStatic();
    end

    function r = cannotCallBasePrivateStatic()
      try
        ClassdefBase.privateStatic();
        r = 'allowed';
      catch
        r = 'blocked';
      end
    end
  end
end
