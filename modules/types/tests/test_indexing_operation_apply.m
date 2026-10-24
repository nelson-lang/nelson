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
% x.(indexOp) applies a nelson.indexing.IndexingOperation (scalar or array) to a
% value as a subsref (read) / subsasgn (write) chain, matching expected behavior
% hook idiom obj.Data.(indexOp). Handled entirely in the engine.
%=============================================================================
% scalar paren read
v = [10 20 30];
op = nelson.indexing.IndexingOperation('Paren', {2});
assert_isequal(v.(op), 20);
%=============================================================================
% scalar dot read
s.field = 42;
assert_isequal(s.(nelson.indexing.IndexingOperation('Dot', {}, 'field')), 42);
%=============================================================================
% scalar brace read
c = {11, 22, 33};
assert_isequal(c.(nelson.indexing.IndexingOperation('Brace', {3})), 33);
%=============================================================================
% chained array read: d(2).f
d = struct('f', {[1 2 3], [4 5 6 7]});
ops = [nelson.indexing.IndexingOperation('Paren', {2}), ...
  nelson.indexing.IndexingOperation('Dot', {}, 'f')];
assert_isequal(d.(ops), [4 5 6 7]);
%=============================================================================
% deeper chain: d(2).f(3)
ops3 = [nelson.indexing.IndexingOperation('Paren', {2}), ...
  nelson.indexing.IndexingOperation('Dot', {}, 'f'), ...
  nelson.indexing.IndexingOperation('Paren', {3})];
assert_isequal(d.(ops3), 6);
%=============================================================================
% scalar paren write
w = [10 20 30];
w.(nelson.indexing.IndexingOperation('Paren', {2})) = 99;
assert_isequal(w, [10 99 30]);
%=============================================================================
% scalar dot write
t.field = 1;
t.(nelson.indexing.IndexingOperation('Dot', {}, 'field')) = 7;
assert_isequal(t.field, 7);
%=============================================================================
% chained write: e(2).f = ...
e = struct('f', {[1 2 3], [4 5 6 7]});
e.(ops) = [8 9];
assert_isequal(e(2).f, [8 9]);
%=============================================================================
% multi-dimensional paren index
m = magic(4);
assert_isequal(m.(nelson.indexing.IndexingOperation('Paren', {2, 3})), m(2, 3));
%=============================================================================
