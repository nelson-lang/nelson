%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/memory_manager/tests/']);
for i = 1:30
  r = test_persistent_function();
end
assert_isequal(r, 29);
clear functions
r = test_persistent_function();
assert_isequal(r, 0);
