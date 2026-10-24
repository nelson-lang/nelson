%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefDuplicateMethod
  methods
    function r = repeated(obj)
      r = 1;
    end

    function r = repeated(obj)
      r = 2;
    end
  end
end
