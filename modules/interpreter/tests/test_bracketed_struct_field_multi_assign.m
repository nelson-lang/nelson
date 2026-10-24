%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% A bracketed left-hand side `[s.field]` over a struct array expands to one
% assignment target per element: the right-hand side is called with numel(s)
% outputs and each value is written to s(k).field.
%=============================================================================
s = struct('x', {1, 2});
[s.y] = deal(10, 20);
assert_isequal(s(1).y, 10);
assert_isequal(s(2).y, 20);
%=============================================================================
% deal with a single input replicates it to every element.
%=============================================================================
clear s
s(1).a = 1; s(2).a = 2; s(3).a = 3;
[s.b] = deal(7);
assert_isequal(s(1).b, 7);
assert_isequal(s(2).b, 7);
assert_isequal(s(3).b, 7);
%=============================================================================
% Column (Nx1) struct arrays distribute the same way.
%=============================================================================
clear c
c(1, 1).v = 1; c(2, 1).v = 2; c(3, 1).v = 3;
[c.w] = deal(4, 5, 6);
assert_isequal(c(1).w, 4);
assert_isequal(c(2).w, 5);
assert_isequal(c(3).w, 6);
%=============================================================================
% Any multiple-output right-hand side works, not only deal.
%=============================================================================
clear s
s(1).f = 0; s(2).f = 0;
[s.f] = size(zeros(2, 3));
assert_isequal(s(1).f, 2);
assert_isequal(s(2).f, 3);
%=============================================================================
% A scalar struct target keeps the single-assignment behavior.
%=============================================================================
clear z
z.a = 1;
[z.b] = deal(99);
assert_isequal(z.b, 99);
%=============================================================================
% Existing field values are overwritten.
%=============================================================================
clear m
m(1).f = 100; m(2).f = 200;
[m.f] = deal(1, 2);
assert_isequal(m(1).f, 1);
assert_isequal(m(2).f, 2);
%=============================================================================
% Dynamic field names `[s.(name)]` distribute as well.
%=============================================================================
clear d
d(1).p = 1; d(2).p = 2;
fn = 'q';
[d.(fn)] = deal(11, 22);
assert_isequal(d(1).q, 11);
assert_isequal(d(2).q, 22);
%=============================================================================
% An undefined target is grown into a 1x1 struct (single output requested).
%=============================================================================
clear nu
[nu.x] = deal(5);
assert_isequal(nu.x, 5);
assert_isequal(numel(nu), 1);
%=============================================================================
% Reading `[s.field]` as a comma-list still works.
%=============================================================================
r = struct('x', {3, 4, 5});
assert_isequal([r.x], [3 4 5]);
%=============================================================================
% Too few right-hand values for the number of targets is an error.
%=============================================================================
clear s
s(1).a = 1; s(2).a = 1; s(3).a = 1;
assert_checkerror('[s.b] = deal(1, 2);', _('The number of outputs should match the number of inputs.'));
%=============================================================================
% Non comma-list targets keep the ordinary multiple-output behavior.
%=============================================================================
[p, q] = deal(2, 1);
assert_isequal(p, 2);
assert_isequal(q, 1);
%=============================================================================
% A single brace comma-list target is unaffected.
%=============================================================================
D = {0, 0};
[D{1:2}] = deal(10, 20);
assert_isequal(D, {10, 20});
%=============================================================================
% A bracketed single indexed-field target `[s(k).field]` assigns that one
% element's field, exactly like the non-bracketed `s(k).field = value`.
%=============================================================================
clear s
s(1).f = 0; s(2).f = 0;
[s(2).f] = deal(42);
assert_isequal(s(1).f, 0);
assert_isequal(s(2).f, 42);
%=============================================================================
clear s
s(1).f = 0; s(2).f = 0;
[s(1).f] = deal(7);
assert_isequal(s(1).f, 7);
assert_isequal(s(2).f, 0);
%=============================================================================
% A bracketed indexed-field target grows the struct array when needed.
%=============================================================================
clear s
s(1).f = 1;
[s(3).f] = deal(9);
assert_isequal(s(1).f, 1);
assert_isequal(s(3).f, 9);
assert_isequal(numel(s), 3);
%=============================================================================
% Any single-output right-hand side works, not only deal.
%=============================================================================
clear s
s(1).f = 0; s(2).f = 0;
[s(2).f] = max(3, 8);
assert_isequal(s(2).f, 8);
%=============================================================================
% A bracketed nested dot chain `[t.a.b]` assigns the leaf field.
%=============================================================================
clear t
t.a.b = 0;
[t.a.b] = deal(77);
assert_isequal(t.a.b, 77);
%=============================================================================
% A bracketed cell-content field `[c{k}.field]` assigns that element's field.
%=============================================================================
clear c
c = {struct('f', 0)};
[c{1}.f] = deal(3);
assert_isequal(c{1}.f, 3);
%=============================================================================
% A bracketed multi-target mixing an indexed-field target with a plain one.
%=============================================================================
clear a b
a(1).f = 0; a(2).f = 0;
[a(2).f, b] = deal(1, 2);
assert_isequal(a(2).f, 1);
assert_isequal(b, 2);
%=============================================================================
