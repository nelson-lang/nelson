%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefProtectedSaveLoadHooks
  properties
    Value = 0
  end

  methods
    function obj = ClassdefProtectedSaveLoadHooks(value)
      if nargin > 0
        obj.Value = value;
      end
    end
  end

  methods (Access = protected)
    function data = saveObjectImpl(obj)
      data = struct('Value', obj.Value + 200);
    end
  end

  methods (Static, Access = protected)
    function obj = loadObjectImpl(data)
      obj = ClassdefProtectedSaveLoadHooks(data.Value - 180);
    end
  end
end
