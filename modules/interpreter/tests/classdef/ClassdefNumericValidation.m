%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefNumericValidation < handle
  properties
    DoubleValue(1, 1) double = uint8(1)
    DoubleRow(1, 2) double = uint8(2)
    SingleValue(1, 1) single = 1
    IntValue(1, 1) int32 = 1
    LogicalValue(1, 1) logical = 1
    LogicalRow(1, 2) logical = 1
  end
end
