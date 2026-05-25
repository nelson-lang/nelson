function classdefCounterDestroyStateCallback(src, eventData)
  global CLASSDEF_COUNTER_DESTROY_STATE_LOG;
  if isvalid(src)
    state = 'valid';
  else
    state = 'invalid';
  end
  CLASSDEF_COUNTER_DESTROY_STATE_LOG = [eventData.EventName, ':', state, ':', int2str(src.Count)];
end
