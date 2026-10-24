%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefDoubleBase < double
  properties
    Unit = ''
  end
  methods
    function obj = ClassdefDoubleBase(v, unit)
      if nargin < 1
        v = [];
      end
      obj = obj@double(v);
      if nargin > 1
        obj.Unit = unit;
      end
    end
  end
end
