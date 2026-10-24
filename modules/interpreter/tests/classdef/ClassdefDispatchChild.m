%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefDispatchChild < ClassdefDispatchBase
  properties
    ChildValue = 2
  end

  methods
    function obj = ClassdefDispatchChild(value, childValue)
      if nargin > 0
        obj.Value = value;
      end
      if nargin > 1
        obj.ChildValue = childValue;
      end
    end

    function r = identify(obj)
      r = ['child:', int2str(obj.Value), ':', int2str(obj.ChildValue)];
    end
  end

  methods (Static)
    function r = staticName()
      r = 'child-static';
    end
  end
end
