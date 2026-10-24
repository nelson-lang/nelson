%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefSiblingHandle
  properties
    v = 0
  end
  methods
    function obj = ClassdefSiblingHandle(x)
      if nargin > 0
        obj.v = x;
      end
    end
    function r = tagit(obj)
      r = obj.v > 0;
    end
    function r = viacell(varargin)
      r = cellfun(@tagit, varargin);
    end
  end
end
%=============================================================================
