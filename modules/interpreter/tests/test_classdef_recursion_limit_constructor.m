%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test, '-begin');
old_recursion_depth = max_recursion_depth();
cleanup_depth = onCleanup(@() max_recursion_depth(old_recursion_depth));
cleanup_path = onCleanup(@() rmpath(path_test));
%=============================================================================
% Allow the constructor three additional frames, independently of the test
% runner's own call depth (isolated worker or in-process execution).
max_recursion_depth(length(dbstack()) + 3);
obj = ClassdefRecursionLimitConstructor(42);
asserts.isequal(class(obj), 'ClassdefRecursionLimitConstructor');
asserts.isequal(obj.Value, 42);
%=============================================================================
