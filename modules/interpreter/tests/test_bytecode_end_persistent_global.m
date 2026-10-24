%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
addpath([nelsonroot(), '/modules/interpreter/tests/']);
%=============================================================================
% end indexing a global/persistent variable, with end nested in the index
% expression, must work (regression: used to raise "END keyword illegal!").
r = bytecode_end_persistent_global_fun();
%=============================================================================
% global
asserts.isequal(r.g_nested, [20 30 40]);
asserts.isequal(r.g_direct, 40);
asserts.isequal(r.g_range, [30 40]);
%=============================================================================
% persistent read with nested end
asserts.isequal(r.p_read, [8 11]);
%=============================================================================
% persistent in-place assignment with nested end on both sides
asserts.isequal(r.p_assigned, [1 4 7 10; 2 8 11 11; 3 6 9 12]);
%=============================================================================
% persistent end-indexed assignment inside a loop
asserts.isequal(r.p_looped, [0 4 7 10; 0 8 11 11; 0 0 0 0]);
%=============================================================================
% a function call whose argument contains a nested end-indexed read of
% another variable must still be dispatched as a call (not an index).
x = [1 2 3 4 5];
asserts.isequal(sum(x(2:end)), 14);
asserts.isequal(max(x(1:end - 1)), 4);
%=============================================================================
