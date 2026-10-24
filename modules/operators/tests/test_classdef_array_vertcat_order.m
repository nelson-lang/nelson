%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Vertical concatenation of struct and classdef object arrays keeps the
% column-major element order: [a b; c d] stores a, c, b, d.
addpath(modulepath('operators', 'tests'));
V = @(x) VertcatOrderValueFixture(x);
H = @(x) VertcatOrderHandleFixture(x);
getv = @(o) o.v;
%=============================================================================
w = [V(1) V(2); V(3) V(4)];
assert_isequal(builtin('size', w), [2 2]);
assert_isequal([w(2, 1).v, w(1, 2).v], [3 2]);
assert_isequal(arrayfun(getv, w), [1 2; 3 4]);
assert_isequal([w(1).v, w(2).v, w(3).v, w(4).v], [1 3 2 4]);
%=============================================================================
w = vertcat([V(1) V(2)], [V(3) V(4)]);
assert_isequal(arrayfun(getv, w), [1 2; 3 4]);
w = cat(1, [V(1) V(2)], [V(3) V(4)]);
assert_isequal(arrayfun(getv, w), [1 2; 3 4]);
%=============================================================================
w = [V(1) V(2); V(3) V(4); V(5) V(6)];
assert_isequal(arrayfun(getv, w), [1 2; 3 4; 5 6]);
w = [[V(1); V(2)], [V(3); V(4)]; V(5) V(6)];
assert_isequal(arrayfun(getv, w), [1 3; 2 4; 5 6]);
w = [V(1); V(2)];
assert_isequal(arrayfun(getv, w), [1; 2]);
%=============================================================================
w = [[V(1); V(2)], [V(3); V(4)]];
assert_isequal(arrayfun(getv, w), [1 3; 2 4]);
w = horzcat([V(1); V(2)], [V(3); V(4)]);
assert_isequal(arrayfun(getv, w), [1 3; 2 4]);
w = cat(2, [V(1); V(2)], [V(3); V(4)]);
assert_isequal(arrayfun(getv, w), [1 3; 2 4]);
%=============================================================================
w = [H(1) H(2); H(3) H(4)];
assert_isequal(arrayfun(getv, w), [1 2; 3 4]);
w = cat(1, [H(1) H(2)], [H(3) H(4)]);
assert_isequal(arrayfun(getv, w), [1 2; 3 4]);
%=============================================================================
S = @(x) struct('v', x);
s = [S(1) S(2); S(3) S(4)];
assert_isequal([s(2, 1).v, s(1, 2).v], [3 2]);
assert_isequal(arrayfun(getv, s), [1 2; 3 4]);
s = vertcat([S(1) S(2)], [S(3) S(4)]);
assert_isequal(arrayfun(getv, s), [1 2; 3 4]);
s = cat(1, [S(1) S(2)], [S(3) S(4)]);
assert_isequal(arrayfun(getv, s), [1 2; 3 4]);
s = [[S(1); S(2)], [S(3); S(4)]];
assert_isequal(arrayfun(getv, s), [1 3; 2 4]);
%=============================================================================
t = [timeseries(1) timeseries(2); timeseries(3) timeseries(4)];
assert_isequal(builtin('size', t), [2 2]);
d = zeros(1, 4);
for k = 1:4
  e = builtin('subsref', t, substruct('()', {k}));
  d(k) = e.Data;
end
assert_isequal(d, [1 3 2 4]);
%=============================================================================
