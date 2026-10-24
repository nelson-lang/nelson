%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefConvertTarget
  properties
    tag = 'target';
    payload = 0;
  end
  methods
    function o = ClassdefConvertTarget(p)
      if nargin > 0
        o.payload = p;
      end
    end
  end
end
%=============================================================================
