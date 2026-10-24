%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Building a scalar structure one field at a time (the amortized in-place
% append path) must stay correct across the field-index hash threshold, when
% reshaped into a structure array, and under copy-on-write aliasing.
%=============================================================================
% scalar struct grown well past the field-index hash threshold
s = struct();
for i = 1:100
  s.(sprintf('f%d', i)) = i * 3;
end
asserts.isequal(numel(fieldnames(s)), 100);
asserts.isequal(s.f1, 3);
asserts.isequal(s.f100, 300);
for i = 1:100
  asserts.isequal(s.(sprintf('f%d', i)), i * 3);
end
%=============================================================================
% a grown scalar struct promoted to a struct array keeps every element's data
t = struct();
t.a = 1;
t.b = 2;
t.c = 3;
t(2).a = 10;
t(2).b = 20;
t(2).c = 30;
asserts.isequal(size(t), [1 2]);
asserts.isequal([t.a], [1 10]);
asserts.isequal([t.b], [2 20]);
asserts.isequal([t.c], [3 30]);
%=============================================================================
% adding a field to every element of a multi-element struct array (no data loss)
clear u
u(1).x = 1;
u(2).x = 2;
u(3).x = 3;
for k = 1:3
  u(k).y = u(k).x * 100;
end
asserts.isequal([u.x], [1 2 3]);
asserts.isequal([u.y], [100 200 300]);
%=============================================================================
% rmfield / orderfields / getfield / setfield after fast-path growth
v = struct();
for i = 1:20
  v.(sprintf('k%d', i)) = i;
end
v = rmfield(v, 'k10');
asserts.isfalse(isfield(v, 'k10'));
asserts.isequal(getfield(v, 'k11'), 11);
v = setfield(v, 'k10', 999);
asserts.isequal(v.k10, 999);
w = orderfields(v);
asserts.isequal(numel(fieldnames(w)), 20);
asserts.isequal(w.k1, 1);
asserts.isequal(w.k10, 999);
asserts.isequal(w.k20, 20);
%=============================================================================
% copy-on-write: adding or changing a field after aliasing must not leak
a = struct('p', 1, 'q', 2);
b = a;
a.r = 3;
asserts.istrue(isfield(a, 'r'));
asserts.isfalse(isfield(b, 'r'));
b.p = 111;
asserts.isequal(a.p, 1);
asserts.isequal(b.p, 111);
%=============================================================================
% overwriting existing fields repeatedly keeps every value in sync
e = struct();
for i = 1:40
  e.(sprintf('g%d', i)) = 0;
end
for i = 1:40
  e.(sprintf('g%d', i)) = i;
end
for i = 1:40
  asserts.isequal(e.(sprintf('g%d', i)), i);
end
%=============================================================================
