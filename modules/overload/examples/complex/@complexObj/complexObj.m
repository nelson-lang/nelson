%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef complexObj
  properties
    r
    i
  end

  methods
    function obj = complexObj(a, b)
      if nargin == 0
        a = [];
        b = [];
      elseif nargin ~= 2
        error('Nelson:complexObj:twoInputsExpected', 'Two input arguments expected.');
      end
      obj.r = a;
      obj.i = b;
    end
  end
end
%=============================================================================
