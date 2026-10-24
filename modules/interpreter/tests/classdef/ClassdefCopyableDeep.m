%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefCopyableDeep < nelson.mixin.Copyable
  properties
    Value = 0
    Child = []
  end
  methods (Access = protected)
    function cp = copyElement(obj)
      % Custom deep copy: the handle-valued Child is copied by value, not
      % shared by reference.
      cp = ClassdefCopyableDeep();
      cp.Value = obj.Value;
      if ~isempty(obj.Child)
        cp.Child = copy(obj.Child);
      end
    end
  end
end
%=============================================================================
