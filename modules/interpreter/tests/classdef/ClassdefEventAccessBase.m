%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefEventAccessBase < handle
  properties
    LastEvent = ''
  end

  events
    PublicEvent
  end

  events (Hidden)
    HiddenEvent
  end

  events (NotifyAccess = private, ListenAccess = private)
    PrivateEvent
  end

  events (NotifyAccess = protected, ListenAccess = protected)
    ProtectedEvent
  end

  methods
    function triggerPublic(obj)
      notify(obj, 'PublicEvent');
    end

    function triggerPrivate(obj)
      notify(obj, 'PrivateEvent');
    end

    function listener = listenPrivate(obj)
      listener = addlistener(obj, 'PrivateEvent', @(src, eventData) obj.recordEvent(eventData.EventName));
    end

    function recordEvent(obj, eventName)
      obj.LastEvent = eventName;
    end
  end
end
