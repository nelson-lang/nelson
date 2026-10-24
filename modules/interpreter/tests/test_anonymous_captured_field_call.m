%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% An anonymous function may call a function handle stored in a field of a
% captured variable, including when the field name starts with an uppercase
% letter (which must not be mistaken for a classdef reference).
s.For = @(x) x * 2;
f = @(y) s.For(y);
assert_isequal(f(5), 10);
%=============================================================================
g = @(y) s.For(y) + 1;
assert_isequal(g(5), 11);
%=============================================================================
% Lowercase field name (control).
s.inc = @(x) x + 1;
h = @(y) s.inc(y);
assert_isequal(h(5), 6);
%=============================================================================
% Nested fields with an uppercase leaf.
a.b.Scale = @(x) x * 3;
k = @(y) a.b.Scale(y);
assert_isequal(k(2), 6);
%=============================================================================
% A captured handle stored in an uppercase field.
data.Handle = @sin;
w = @(x) data.Handle(x);
assert_isequal(w(0), 0);
%=============================================================================
% Field data access (no call) on a captured variable still works.
p.Value = 42;
q = @(y) p.Value + y;
assert_isequal(q(8), 50);
%=============================================================================
