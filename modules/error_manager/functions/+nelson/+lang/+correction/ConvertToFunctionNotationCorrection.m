%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ConvertToFunctionNotationCorrection
  properties (SetAccess = private)
    Method
  end
  methods
    function obj = ConvertToFunctionNotationCorrection(method)
      obj.Method = method;
    end
  end
end
%=============================================================================
