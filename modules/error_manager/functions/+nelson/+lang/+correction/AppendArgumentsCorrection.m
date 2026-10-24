%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef AppendArgumentsCorrection
  properties (SetAccess = private)
    Arguments
  end
  methods
    function obj = AppendArgumentsCorrection(arguments)
      obj.Arguments = arguments;
    end
  end
end
%=============================================================================
