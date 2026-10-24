%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefEventAccessChild < ClassdefEventAccessBase
  methods
    function triggerProtected(obj)
      notify(obj, 'ProtectedEvent');
    end

    function listener = listenProtected(obj)
      listener = addlistener(obj, 'ProtectedEvent', @(src, eventData) obj.recordEvent(eventData.EventName));
    end
  end
end
