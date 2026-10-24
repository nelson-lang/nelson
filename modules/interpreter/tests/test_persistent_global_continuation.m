%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A 'persistent' or 'global' declaration may be split over several physical
% lines with a '...' line continuation. Before the fix the '...' was taken as
% a literal command argument and raised 'Valid variable name expected.'.
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
%=============================================================================
r = persistent_continuation_fixture();
assert_isequal(r, [1, 2, 3, 4, 5, 6]);
%=============================================================================
clear global gc1 gc2 gc3 gc4
global_continuation_fixture();
global gc1 gc2 gc3 gc4
assert_isequal([gc1, gc2, gc3, gc4], [10, 20, 30, 40]);
clear global gc1 gc2 gc3 gc4
%=============================================================================
