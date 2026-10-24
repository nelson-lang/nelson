%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% Scalar copy is an independent handle.
a = ClassdefCopyableArray(5);
b = copy(a);
b.Value = 9;
asserts.isequal(a.Value, 5);
asserts.isequal(b.Value, 9);
asserts.istrue(a ~= b);
asserts.isequal(class(b), 'ClassdefCopyableArray');
%=============================================================================
% copy() of a row array returns an array of independent copies (regression:
% used to error "Wrong type for argument. Handle expected.").
arr(1) = ClassdefCopyableArray(10);
arr(2) = ClassdefCopyableArray(20);
arr(3) = ClassdefCopyableArray(30);
brr = copy(arr);
asserts.isequal(size(brr), [1, 3]);
brr(2).Value = -1;
asserts.isequal(arr(2).Value, 20);
asserts.isequal(brr(2).Value, -1);
asserts.isequal(brr(3).Value, 30);
asserts.istrue(arr(1) ~= brr(1));
%=============================================================================
% copy() preserves the shape of a matrix of handles.
m(1, 1) = ClassdefCopyableArray(1);
m(1, 2) = ClassdefCopyableArray(2);
m(2, 1) = ClassdefCopyableArray(3);
m(2, 2) = ClassdefCopyableArray(4);
cm = copy(m);
asserts.isequal(size(cm), [2, 2]);
cm(2, 2).Value = 99;
asserts.isequal(m(2, 2).Value, 4);
asserts.isequal(cm(2, 2).Value, 99);
%=============================================================================
% copy() of an empty handle array is empty.
e = ClassdefCopyableArray.empty(0, 0);
f = copy(e);
asserts.istrue(isempty(f));
%=============================================================================
% Default shallow copy shares handle-valued properties by reference.
shared = ClassdefCopyableArray(42);
h = ClassdefCopyableArray(1);
h.Ref = shared;
hc = copy(h);
asserts.istrue(hc.Ref == h.Ref);
hc.Ref.Value = 100;
asserts.isequal(shared.Value, 100);
%=============================================================================
% A subclass copyElement override customises the copy (deep copy of Child)
% (regression: the override used to be ignored, yielding a shallow ref copy).
p = ClassdefCopyableDeep();
p.Value = 1;
p.Child = ClassdefCopyableDeep();
p.Child.Value = 10;
q = copy(p);
q.Child.Value = 999;
asserts.isequal(p.Child.Value, 10);
asserts.isequal(q.Value, 1);
%=============================================================================
% copyElement override is applied per element for arrays too.
da(1) = ClassdefCopyableDeep();
da(2) = ClassdefCopyableDeep();
da(1).Child = ClassdefCopyableDeep();
da(1).Child.Value = 7;
dc = copy(da);
asserts.isequal(size(dc), [1, 2]);
dc(1).Child.Value = 123;
asserts.isequal(da(1).Child.Value, 7);
%=============================================================================
