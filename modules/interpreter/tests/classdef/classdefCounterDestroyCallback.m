function classdefCounterDestroyCallback(src, eventData)
  global CLASSDEF_COUNTER_DELETE_LOG;
  CLASSDEF_COUNTER_DELETE_LOG = [CLASSDEF_COUNTER_DELETE_LOG, eventData.EventName];
end
