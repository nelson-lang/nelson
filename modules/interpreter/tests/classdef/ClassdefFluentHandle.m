%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefFluentHandle < handle
  properties
    Trail = {}
    Value = 0
  end
  methods
    function obj = ClassdefFluentHandle(v)
      if nargin > 0
        obj.Value = v;
      end
    end
    function obj = tag(obj, t)
      obj.Trail{end + 1} = t;
    end
    function obj = add(obj, k)
      obj.Value = obj.Value + k;
    end
    function r = total(obj)
      r = obj.Value;
    end
  end
end
%=============================================================================
