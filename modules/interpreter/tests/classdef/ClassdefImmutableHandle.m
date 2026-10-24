%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefImmutableHandle < handle
  properties (SetAccess = immutable)
    Id
  end
  methods
    function obj = ClassdefImmutableHandle(id)
      if nargin >= 1
        obj.Id = id;
      end
    end
  end
end
