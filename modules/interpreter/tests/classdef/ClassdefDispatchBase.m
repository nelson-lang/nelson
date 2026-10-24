%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefDispatchBase
  properties
    Value = 1
  end

  methods
    function obj = ClassdefDispatchBase(value)
      if nargin > 0
        obj.Value = value;
      end
    end

    function r = identify(obj)
      r = ['base:', int2str(obj.Value)];
    end

    function r = callIdentifyDot(obj)
      r = obj.identify();
    end

    function r = callIdentifyFunction(obj)
      r = identify(obj);
    end

    function r = inheritedOnly(obj)
      r = ['inherited:', int2str(obj.Value)];
    end
  end

  methods (Static)
    function r = staticName()
      r = 'base-static';
    end

    function r = staticOnly()
      r = 'base-only';
    end
  end
end
