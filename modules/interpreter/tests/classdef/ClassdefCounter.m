classdef ClassdefCounter < handle
  properties
    Count = 0
    LastEvent = ''
  end

  properties (Access = private)
    SecretCount = -5
  end

  properties (Access = protected)
    ProtectedCount = 100
  end

  properties (GetAccess = public, SetAccess = private)
    ReadOnlyCount = 77
  end

  properties (GetAccess = private, SetAccess = public)
    WriteOnlyCount = 88
  end

  events
    CountChanged
  end

  methods
    function obj = ClassdefCounter(value)
      if nargin > 0
        obj.Count = value;
      end
    end

    function increment(obj, step)
      if nargin < 2
        step = 1;
      end
      obj.Count = obj.Count + step;
      notify(obj, 'CountChanged');
    end

    function recordEvent(obj, name)
      obj.LastEvent = name;
    end

    function r = secretCount(obj)
      r = obj.SecretCount;
    end

    function setSecretCount(obj, value)
      obj.SecretCount = value;
    end

    function r = protectedCount(obj)
      r = obj.ProtectedCount;
    end

    function setReadOnlyCount(obj, value)
      obj.ReadOnlyCount = value;
    end

    function r = writeOnlyCount(obj)
      r = obj.WriteOnlyCount;
    end

    function delete(obj)
      global CLASSDEF_COUNTER_DELETE_LOG;
      CLASSDEF_COUNTER_DELETE_LOG = [CLASSDEF_COUNTER_DELETE_LOG, 'delete'];
    end
  end

  methods (Static)
    function obj = zero()
      obj = ClassdefCounter(0);
    end
  end
end
