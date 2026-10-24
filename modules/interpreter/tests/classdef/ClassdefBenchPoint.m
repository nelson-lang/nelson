%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefBenchPoint
  properties
    Id = 0
  end
  methods
    function obj = ClassdefBenchPoint(id)
      if nargin > 0
        obj.Id = id;
      end
    end
    function result = plus(left, right)
      result = ClassdefBenchPoint(left.Id + right.Id);
    end
    function id = getId(obj)
      id = obj.Id;
    end
    function result = addTo(obj, value)
      result = obj.Id + value;
    end
  end
end
%=============================================================================
