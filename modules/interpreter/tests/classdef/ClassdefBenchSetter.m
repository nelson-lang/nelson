%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefBenchSetter
  properties
    Id = 0
  end
  methods
    function obj = ClassdefBenchSetter(id)
      if nargin > 0
        obj.Id = id;
      end
    end
    function obj = set.Id(obj, value)
      obj.Id = value;
    end
    function value = get.Id(obj)
      value = obj.Id;
    end
  end
end
%=============================================================================
