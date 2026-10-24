%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% a class without constructor calls the superclass constructor with no argument
c = ClassdefImplicitCtorChild();
asserts.isequal(class(c), 'ClassdefImplicitCtorChild');
asserts.isequal(c.v, 42);
asserts.isequal(c.calls, 1);
asserts.isequal(c.extra, 3);
%=============================================================================
% intermediate class without constructor: the base constructor runs once
g = ClassdefImplicitCtorGrandChild();
asserts.isequal(class(g), 'ClassdefImplicitCtorGrandChild');
asserts.isequal(g.v, 42);
asserts.isequal(g.calls, 1);
asserts.isequal(g.extra, 3);
%=============================================================================
% handle class
h = ClassdefImplicitCtorHandleChild();
asserts.isequal(class(h), 'ClassdefImplicitCtorHandleChild');
asserts.isequal(h.v, 'initialized');
asserts.istrue(isa(h, 'handle'));
h2 = h;
h2.v = 'changed';
asserts.isequal(h.v, 'changed');
%=============================================================================
% explicit constructor without superclass call: the superclass constructor
% runs with no argument before the constructor body
e = ClassdefImplicitCtorExplicitChild();
asserts.isequal(e.v, 42);
asserts.isequal(e.w, 43);
asserts.isequal(e.calls, 1);
%=============================================================================
% explicit superclass call: no additional implicit call
x = ClassdefImplicitCtorExplicitCall();
asserts.isequal(x.v, 84);
asserts.isequal(x.calls, 1);
%=============================================================================
hx = ClassdefImplicitCtorHandleExplicitChild('abc');
asserts.isequal(hx.v, 'initialized');
asserts.isequal(hx.w, 'initialized-abc');
%=============================================================================
