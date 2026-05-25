classdef ClassdefObservableCounter < handle
  properties (GetObservable, SetObservable)
    Count = 0
  end

  properties
    LastEvent = ''
    LastProperty = ''
  end

  methods
    function obj = ClassdefObservableCounter(value)
      if nargin > 0
        obj.Count = value;
      end
    end

    function recordPropertyEvent(obj, eventData)
      obj.LastEvent = eventData.EventName;
      obj.LastProperty = eventData.PropertyName;
    end
  end
end
