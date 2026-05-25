classdef ClassdefBase
  properties
    BaseValue = 10
  end

  properties (Constant)
    BaseConstant = 7
  end

  properties (Access = private)
    BasePrivateValue = 31
  end

  properties (Access = protected)
    BaseProtectedValue = 41
  end

  properties (GetAccess = public, SetAccess = private)
    BaseReadOnlyByAccess = 51
  end

  properties (GetAccess = private, SetAccess = public)
    BaseWriteOnlyPublic = 61
  end

  events
    BaseChanged
  end

  methods
    function r = baseMethod(obj)
      r = obj.BaseValue;
    end

    function r = callPrivate(obj)
      r = privateMethod(obj) + obj.privateMethod();
    end

    function r = readPrivateProperty(obj)
      r = obj.BasePrivateValue;
    end

    function obj = setPrivateProperty(obj, value)
      obj.BasePrivateValue = value;
    end

    function r = readWriteOnlyProperty(obj)
      r = obj.BaseWriteOnlyPublic;
    end

    function obj = setReadOnlyProperty(obj, value)
      obj.BaseReadOnlyByAccess = value;
    end
  end

  methods (Static)
    function r = baseStatic()
      r = 'base';
    end

    function r = callPrivateStatic()
      r = ClassdefBase.privateStatic();
    end
  end

  methods (Hidden)
    function r = hiddenMethod(obj)
      r = 99;
    end
  end

  methods (Sealed)
    function r = sealedMethod(obj)
      r = 'sealed';
    end
  end

  methods (Access = protected)
    function r = protectedMethod(obj)
      r = 21;
    end
  end

  methods (Static, Access = protected)
    function r = protectedStatic()
      r = 'protectedStatic';
    end
  end

  methods (Access = private)
    function r = privateMethod(obj)
      r = -1;
    end
  end

  methods (Static, Access = private)
    function r = privateStatic()
      r = 'privateStatic';
    end
  end
end
