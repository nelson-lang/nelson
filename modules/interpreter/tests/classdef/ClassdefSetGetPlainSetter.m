%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefSetGetPlainSetter < nelson.mixin.SetGet
  properties
    Name = ''
    Count = 0
  end
  methods
    function set.Name(obj, value)
      obj.Count = obj.Count + 1;
      obj.Name = upper(char(value));
    end
  end
end
%=============================================================================
