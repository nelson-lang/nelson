%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefHandlePlainSetter < handle
  properties
    Flag = false
    Level = 0
    Twice = 0
    Plain = 0
    Calls = {}
  end
  properties (SetObservable)
    Watched = 0
  end
  methods
    function set.Flag(obj, value)
      obj.Calls{end + 1} = 'Flag';
      obj.Flag = logical(value);
    end
    function obj = set.Level(obj, value)
      obj.Calls{end + 1} = 'Level';
      obj.Level = max(value, 0);
    end
    function set.Twice(obj, value)
      obj.Calls{end + 1} = 'Twice';
      obj.Twice = 2 * value;
      obj.Flag = value;
    end
    function set.Watched(obj, value)
      obj.Calls{end + 1} = 'Watched';
      obj.Watched = value + 1;
    end
  end
end
%=============================================================================
