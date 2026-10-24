%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefBadSaveHook
  methods
    function data = saveObjectImpl(obj)
      data = 42;
    end
  end

  methods (Static)
    function obj = loadObjectImpl(data)
      obj = ClassdefBadSaveHook();
    end
  end
end
