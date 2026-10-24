%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefContinuationConstructor
  properties
    Value = []
    Other = []
  end

  methods
    function obj = ClassdefContinuationConstructor(value, ...
        other)
      if nargin > 0
        obj.Value = value;
        obj.Other = other;
      end
    end
  end
end
