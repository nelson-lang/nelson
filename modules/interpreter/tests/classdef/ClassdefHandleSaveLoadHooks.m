%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefHandleSaveLoadHooks < handle
  properties
    Value = 0
  end

  methods
    function obj = ClassdefHandleSaveLoadHooks(value)
      if nargin > 0
        obj.Value = value;
      end
    end

    function data = saveObjectImpl(obj)
      global CLASSDEF_HANDLE_SAVE_HOOK_COUNT;
      if isempty(CLASSDEF_HANDLE_SAVE_HOOK_COUNT)
        CLASSDEF_HANDLE_SAVE_HOOK_COUNT = 0;
      end
      CLASSDEF_HANDLE_SAVE_HOOK_COUNT = CLASSDEF_HANDLE_SAVE_HOOK_COUNT + 1;
      data = struct('Value', obj.Value + 10);
    end
  end

  methods (Static)
    function obj = loadObjectImpl(data)
      obj = ClassdefHandleSaveLoadHooks(data.Value + 20);
    end
  end
end
