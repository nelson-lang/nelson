%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/142
% <-- Short Description -->
% clear('functionName') clears all persistent variables of functionName function
%=============================================================================
addpath([nelsonroot(), '/modules/memory_manager/tests/']);
assert_isequal(test_clear_persistent(), 0);
assert_isequal(test_clear_persistent(), 1);
assert_isequal(test_clear_persistent(), 2);
clear test_clear_persistent
assert_isequal(test_clear_persistent(), 0);
assert_isequal(test_clear_persistent(), 1);
assert_isequal(test_clear_persistent(), 2);
