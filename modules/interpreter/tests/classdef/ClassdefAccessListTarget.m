%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefAccessListTarget < handle
  properties
    LastEvent = ''
  end

  properties (GetAccess = {?ClassdefAccessFriend, ?ClassdefAccessPeer}, ...
      SetAccess = ?ClassdefAccessPeer)
    SharedValue = 41
  end

  properties (Constant, GetAccess = {?ClassdefAccessFriend, ?ClassdefAccessPeer})
    SharedConstant = 13
  end

  events (ListenAccess = {?ClassdefAccessFriend, ?ClassdefAccessPeer}, ...
      NotifyAccess = ?ClassdefAccessPeer)
    SharedEvent
  end

  methods
    function obj = ClassdefAccessListTarget(value)
      if nargin > 0
        obj.SharedValue = value;
      end
    end

    function recordEvent(obj, eventName)
      obj.LastEvent = eventName;
    end
  end

  methods (Access = {?ClassdefAccessFriend, ?ClassdefAccessPeer})
    function r = sharedMethod(obj)
      r = obj.SharedValue + 1;
    end
  end

  methods (Static, Access = {?ClassdefAccessFriend, ?ClassdefAccessPeer})
    function r = sharedStatic()
      r = ClassdefAccessListTarget.SharedConstant + 2;
    end
  end
end
