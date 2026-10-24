%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefConflictMethodResolved < ClassdefConflictMethodA & ClassdefConflictMethodB
  methods
    function r = sharedMethod(obj)
      r = 3;
    end
  end
end
