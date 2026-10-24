%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef Options
  properties
    Value = 31
  end

  methods
    function obj = Options(value)
      if nargin > 0
        obj.Value = value;
      end
    end
  end
end
