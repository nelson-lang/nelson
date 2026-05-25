classdef ClassdefSplit
  properties
    Value = 11
  end

  properties (Access = private)
    HiddenValue = 4
  end

  properties (Access = protected)
    ProtectedValue = 8
  end

  methods
    r = callPrivateExternal(obj)
  end

  methods (Static)
    r = staticExternal()
  end

  methods (Access = private)
    r = privateExternal(obj)
  end

  methods (Access = protected)
    r = protectedExternal(obj)
  end
end
