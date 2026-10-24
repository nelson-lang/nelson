%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefContainerValidation < handle
  properties
    Cells(1, 2) cell = {1}
    Structs(1, 2) struct = struct('Value', 2)
    FunctionValue(1, 1) function_handle = @sin
  end
end
