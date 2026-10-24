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
% end keyword in indexed assignments (fused in-place opcodes)
a = [1 2 3];
a(end) = 10;
asserts.isequal(a, [1 2 10]);
a(end + 1) = 4;
asserts.isequal(a, [1 2 10 4]);
a(end + 2) = 7;
asserts.isequal(a, [1 2 10 4 0 7]);
%=============================================================================
c = {1, 2};
c{end} = 5;
asserts.isequal(c, {1, 5});
c{end + 1} = 'x';
asserts.isequal(c, {1, 5, 'x'});
c(end) = [];
asserts.isequal(c, {1, 5});
%=============================================================================
m = magic(4);
m(end, end) = -1;
asserts.isequal(m(4, 4), -1);
m(end + 1, :) = 1:4;
asserts.isequal(size(m, 1), 5);
m(2, end) = 99;
asserts.isequal(m(2, 4), 99);
m(end, :) = [];
asserts.isequal(size(m, 1), 4);
%=============================================================================
v = 1:5;
v(end - 1) = 0;
asserts.isequal(v, [1 2 3 0 5]);
v(end:end + 1) = [8 9];
asserts.isequal(v, [1 2 3 0 8 9]);
v(1:end - 1) = [];
asserts.isequal(v, 9);
%=============================================================================
q = 1:4;
q(end / 2) = 0;
asserts.isequal(q, [1 0 3 4]);
z = 1:6;
z(end - 2:end) = 0;
asserts.isequal(z, [1 2 3 0 0 0]);
w = 1:5;
w(end:-1:1) = w;
asserts.isequal(w, [5 4 3 2 1]);
%=============================================================================
% end referring to a nested indexed variable inside the index expression
idx = [2 3];
t = 10:10:40;
t(idx(end)) = -1;
asserts.isequal(t, [10 20 -1 40]);
%=============================================================================
s = struct('f', {1, 2});
s(end + 1).f = 3;
asserts.isequal(numel(s), 3);
asserts.isequal(s(3).f, 3);
s(end).f = 30;
asserts.isequal(s(3).f, 30);
s(end) = [];
asserts.isequal(numel(s), 2);
%=============================================================================
% undefined base: end evaluates to 0
clear u;
u(end + 1) = 1;
asserts.isequal(u, 1);
clear u;
u{end + 1} = 2;
asserts.isequal(u, {2});
%=============================================================================
% function-local growth loops
r = bytecode_end_indexed_assign_fun(50);
asserts.isequal(r.a, 1:50);
asserts.isequal(r.c, num2cell(1:50));
asserts.isequal(r.n, 50);
asserts.isequal(r.last, 50);
%=============================================================================
% script growing variables inherited from the calling workspace
g = [1 2];
gc = {1};
bytecode_end_indexed_assign_script;
asserts.isequal(g, [1 2 3]);
asserts.isequal(gc, {1, 2});
%=============================================================================
