%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefTextValidation < handle
  properties
    CharValue(1, :) char = "abc"
    TwoChars(1, 2) char = "AB"
    RepeatedChars(1, 2) char = 'A'
    StringValue(1, 1) string = 'abc'
    StringRow(1, 2) string = "A"
    TextScalar(1, 1) string {mustBeTextScalar} = 123
    NonzeroText(1, :) char {mustBeNonzeroLengthText} = "abc"
    TextValue string {mustBeText} = 456
  end
end
