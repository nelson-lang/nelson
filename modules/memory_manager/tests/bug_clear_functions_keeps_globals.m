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
% clear('functions') destroyed all user global variables
%=============================================================================
global bug_clear_fcts_g
bug_clear_fcts_g = 42;
clear('functions');
global bug_clear_fcts_g
asserts.isequal(bug_clear_fcts_g, 42);
clear global bug_clear_fcts_g
%=============================================================================
% persistent variables must still be reset by clear('functions')
addpath([nelsonroot(), '/modules/memory_manager/tests/']);
asserts.isequal(bug_clear_functions_counter(), 1);
asserts.isequal(bug_clear_functions_counter(), 2);
clear('functions');
asserts.isequal(bug_clear_functions_counter(), 1);
%=============================================================================
