%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression test: in a script chunk, a field of a variable that the compiler
% cannot see statically assigned (created through eval / load / assignin) may
% be indexed with a whole-array colon `var.field(:)` or an `end` subscript.
% Such a subscript was wrongly lowered to the runtime member-call opcode
% (CALL_CLASSDEF_MEMBER), where the colon reached the generic expression
% compiler and raised "Bytecode compiler unsupported construct: operator
% expression". It must instead compile as field indexing.
%=============================================================================
% The variable `store` is only ever created inside a string, so the compiler
% never records a local assignment for it: it is resolved from the workspace at
% run time, exactly like a variable produced by load().
eval('store.values = [3 -1 2 8];');
%=============================================================================
% Whole-array colon on a field, used directly inside a function-call argument
% (the shape that previously failed to compile).
assert_isequal(sum(store.values(:)), 12);
assert_isequal(store.values(:)', [3 -1 2 8]);
assert_isequal(numel(store.values(:)), 4);
%=============================================================================
% `end` subscript on a field must also compile as field indexing.
assert_isequal(store.values(end), 8);
%=============================================================================
% Nested field with a colon index.
eval('nested.inner.data = [10 20; 30 40];');
assert_isequal(nested.inner.data(:)', [10 30 20 40]);
%=============================================================================
% A field name starting with an uppercase letter (which could be mistaken for
% a classdef reference) must behave the same way.
eval('bundle.Payload = [5 6 7];');
assert_isequal(bundle.Payload(:)', [5 6 7]);
%=============================================================================
% A genuinely local (statically assigned) variable keeps working as a control.
local.values = [1 2 3 4];
assert_isequal(local.values(:)', [1 2 3 4]);
%=============================================================================
