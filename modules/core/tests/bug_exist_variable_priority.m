%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% exist(name) must return 1 when a variable shadows a function
%=============================================================================
cat = 1;
asserts.isequal(exist('cat'), 1);
clear cat
% cat is a builtin function
asserts.isequal(exist('cat'), 5);
%=============================================================================
bug_exist_no_such_name_xyz = 3;
asserts.isequal(exist('bug_exist_no_such_name_xyz'), 1);
clear bug_exist_no_such_name_xyz
asserts.isequal(exist('bug_exist_no_such_name_xyz'), 0);
%=============================================================================
