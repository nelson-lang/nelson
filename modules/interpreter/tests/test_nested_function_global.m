%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A nested function must see variables declared 'global' in its parent and
% update the shared global, including through indexed assignment. Before the
% fix this raised 'Undefined variable'.
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
clear global nestedGlobalCounter
clear global nestedGlobalGrid
r = nested_function_global_fixture();
assert_isequal(r, [3, 3]);
global nestedGlobalCounter
assert_isequal(nestedGlobalCounter, 3);
clear global nestedGlobalCounter
clear global nestedGlobalGrid
%=============================================================================
