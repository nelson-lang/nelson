%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
s = struct('b', 2, 'c', 3, 'a', 1);
s2 = orderfields(s);
SREF = struct('a', 1, 'b', 2, 'c', 3);
asserts.isequal(s2, SREF);
%=============================================================================
clear ans;
txt = evalc('orderfields(s)');
asserts.istrue(contains(txt, 'ans ='));
asserts.istrue(contains(txt, 'a: 1'));
asserts.istrue(contains(txt, 'b: 2'));
asserts.istrue(contains(txt, 'c: 3'));
%=============================================================================
s = struct('b', 2, 'c', 3, 'a', 1);
[s2, p] = orderfields(s);
SREF = struct('a', 1, 'b', 2, 'c', 3);
PREF = [3; 1; 2];
asserts.isequal(s2, SREF);
asserts.isequal(p, PREF);
%=============================================================================
s = struct('b', 2, 'c', 3, 'a', 1);
[s2, p2] = orderfields(s, {'b', 'a', 'c'});
SREF = struct('b', 2, 'a', 1, 'c', 3);
PREF = [1; 3; 2];
asserts.isequal(s2, SREF);
asserts.isequal(p2, PREF);
%=============================================================================
[s2, p2] = orderfields(s, ["b", "a", "c"]);
SREF = struct('b', 2, 'a', 1, 'c', 3);
PREF = [1; 3; 2];
asserts.isequal(s2, SREF);
asserts.isequal(p2, PREF);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
P = [3 1 2];
s2 = orderfields(s, P);
SREF = struct('a', 4, 'b', 3, 'c', 7);
asserts.isequal(s2, SREF);
%=============================================================================
S = struct('b', 2, 'a', 1);
Ordered = orderfields(S);
asserts.isequal(fieldnames(Ordered), {'a'; 'b'});
[ExplicitOrder, ExplicitPermutation] = orderfields(struct('c', 3, 'a', 1, 'b', 2), {'b', 'c', 'a'});
asserts.isequal(fieldnames(ExplicitOrder), {'b'; 'c'; 'a'});
asserts.isequal(ExplicitPermutation, [3; 1; 2]);
%=============================================================================
S = struct();
[Ordered, Permutation] = orderfields(S);
asserts.isequal(fieldnames(Ordered), cell(0, 1));
asserts.isequal(Permutation, zeros(0, 1));
asserts.isequal(size(Ordered), [1 1]);
%=============================================================================
[Ordered, Permutation] = orderfields(S, struct());
asserts.isequal(fieldnames(Ordered), cell(0, 1));
asserts.isequal(Permutation, zeros(0, 1));
%=============================================================================
[Ordered, Permutation] = orderfields(S, {});
asserts.isequal(fieldnames(Ordered), cell(0, 1));
asserts.isequal(Permutation, zeros(0, 1));
%=============================================================================
[Ordered, Permutation] = orderfields(S, []);
asserts.isequal(fieldnames(Ordered), cell(0, 1));
asserts.isequal(Permutation, zeros(0, 1));
%=============================================================================
S(1, 1).b = 11;
S(1, 1).a = 12;
S(2, 1).b = 21;
S(2, 1).a = 22;
S(1, 2).b = 31;
S(1, 2).a = 32;
S(2, 2).b = 41;
S(2, 2).a = 42;
[Ordered, Permutation] = orderfields(S);
asserts.isequal(fieldnames(Ordered), {'a'; 'b'});
asserts.isequal(Permutation, [2; 1]);
asserts.isequal(size(Ordered), [2 2]);
asserts.isequal(Ordered(2, 1).a, 22);
asserts.isequal(Ordered(1, 2).b, 31);
%=============================================================================
[Ordered, Permutation] = orderfields(struct('b', 2, 'c', 3, 'a', 1), struct('c', 0, 'a', 0, 'b', 0));
asserts.isequal(fieldnames(Ordered), {'c'; 'a'; 'b'});
asserts.isequal(Permutation, [2; 3; 1]);
%=============================================================================
S = struct('b', {}, 'a', {});
[Ordered, Permutation] = orderfields(S);
asserts.isequal(fieldnames(Ordered), {'a'; 'b'});
asserts.isequal(Permutation, [2; 1]);
asserts.isequal(size(Ordered), [0 0]);
%=============================================================================
