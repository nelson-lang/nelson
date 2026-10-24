%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Two levels down: subsref/subsasgn are resolved by walking the full
% superclass chain (grandchild -> child -> base).
classdef ClassdefInheritedIndexGrandchild < ClassdefInheritedIndexChild
  methods
    function p = ClassdefInheritedIndexGrandchild()
      p = p@ClassdefInheritedIndexChild();
    end
  end
end
%=============================================================================
