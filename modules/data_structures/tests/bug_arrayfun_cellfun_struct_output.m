%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: arrayfun/cellfun assembling per-element scalar structures into a
% struct array lost the field values of every element but the first, and a
% following [s.field] field-gather hung (infinite loop) on the corrupted
% storage. The assembly wrote elements 2..N with a raw byte copy that
% shallow-copied the nested field references without owner accounting.
%=============================================================================
s = arrayfun(@(k) struct('x', k, 'y', k * 10), 1:4);
assert_isequal(size(s), [1 4]);
assert_isequal(s(1).x, 1);
assert_isequal(s(3).y, 30);
w = s(2);
assert_isequal(w.x, 2);
assert_isequal([s.x], [1 2 3 4]);
assert_isequal([s.y], [10 20 30 40]);
%=============================================================================
s = cellfun(@(k) struct('x', k), num2cell(1:5));
assert_isequal([s.x], [1 2 3 4 5]);
assert_isequal(s(4).x, 4);
%=============================================================================
% Multiple fields, varied value kinds preserved per element.
s = arrayfun(@(k) struct('a', k, 'b', k * k, 'c', [k, k + 1]), 1:6);
assert_isequal([s.a], 1:6);
assert_isequal([s.b], (1:6) .^ 2);
assert_isequal(s(5).c, [5 6]);
%=============================================================================
% Assembled elements must be independent (no aliasing from the byte copy):
% editing one element must not touch the others.
s = arrayfun(@(k) struct('x', k), 1:3);
s(1).x = 100;
assert_isequal([s.x], [100 2 3]);
%=============================================================================
