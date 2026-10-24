%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
c = cell(1, 3);
asserts.isequal(c, {[], [], []});
c{1} = 42;
asserts.isequal(c{1}, 42);
asserts.isequal(c{2}, []);
asserts.isequal(c{3}, []);
%=============================================================================
s = strings(1, 3);
s(2) = "value";
asserts.isequal(s(2), "value");
asserts.isequal(s(1), "");
asserts.isequal(s(3), "");
%=============================================================================
st(1, 3).field = [];
st(2).field = 7;
asserts.isequal(st(1).field, []);
asserts.isequal(st(2).field, 7);
asserts.isequal(st(3).field, []);
%=============================================================================
