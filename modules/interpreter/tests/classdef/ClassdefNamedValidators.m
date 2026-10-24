%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefNamedValidators
  properties
    Mode {mustBeMember(Mode, {'auto', 'manual'})} = 'auto'
    Level(1, 1) double {mustBeInRange(Level, 1, 10)} = 5
    Count(1, 1) double {mustBeInteger, mustBeGreaterThan(Count, 0)} = 1
    Word(1, 1) string {mustBeMember(Word, ["abc", "def"])} = "abc"
  end
end
