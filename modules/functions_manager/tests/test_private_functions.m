%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
directory_1 = [modulepath('functions_manager', 'tests'), '/test_private/test_private_1'];
directory_2 = [modulepath('functions_manager', 'tests'), '/test_private/test_private_2'];
addpath(directory_2);
addpath(directory_1);
%=============================================================================
R2 = ex_test_2();
REF_R2 = 57;
assert_isequal(R2, REF_R2);
%=============================================================================
R1 = ex_test_1();
REF_R1 = 4;
assert_isequal(R1, REF_R1);
%=============================================================================
