%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefStaticMultiOutput
  methods (Static)
    function [a, b, c] = split(value)
      a = value;
      b = value + 1;
      c = value + 2;
    end

    function v = vector(value)
      v = [value, value + 1, value + 2];
    end
  end
end
