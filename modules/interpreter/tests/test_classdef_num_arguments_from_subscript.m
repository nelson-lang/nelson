%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A class that overrides numArgumentsFromSubscript controls how many values a
% brace indexing produces when expanded as a comma-separated list.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
obj = ClassdefNumArguments({10, 20, 30, 40});
%=============================================================================
% single value indexing is unchanged
asserts.isequal(obj{2}, 20);
%=============================================================================
% comma-separated-list in a concatenation
asserts.isequal([obj{[1 2 3]}], [10 20 30]);
%=============================================================================
% comma-separated-list building a cell array
c = {obj{[1 2 4]}};
asserts.isequal(numel(c), 3);
asserts.isequal(c, {10, 20, 40});
%=============================================================================
% comma-separated-list expanded as function arguments
asserts.isequal(numelArgs(obj{[1 2 3]}), 3);
asserts.isequal(numelArgs(obj{[1 2]}), 2);
asserts.isequal(numelArgs(obj{1}), 1);
%=============================================================================
% left-hand-side driven multi-output still uses the number of targets
[a, b] = obj{[3 4]};
asserts.isequal(a, 30);
asserts.isequal(b, 40);
%=============================================================================
