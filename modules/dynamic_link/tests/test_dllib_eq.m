%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_1 = modulepath('dynamic_link', 'builtin');
path_2 = modulepath('core', 'builtin');
lib1 = dlopen(path_1);
lib2 = dlopen(path_2);
assert_istrue(lib1 == lib1);
assert_isequal([lib1, lib2] == lib1, [true, false]);
%=============================================================================
