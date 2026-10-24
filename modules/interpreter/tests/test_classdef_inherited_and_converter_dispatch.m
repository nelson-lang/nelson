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
% A .m (macro) function called on a subclass dispatches to an overload inherited
% from a superclass through an external @Base/method.m file.
d = ClassdefMacroOverloadDerived(5);
assert_isequal(diff(d), 1005);
%=============================================================================
% The exact class still dispatches to its own overload.
b = ClassdefMacroOverloadBase(7);
assert_isequal(diff(b), 1007);
%=============================================================================
% A bare @name handle to a sibling method dispatches on the class of the
% arguments when invoked through cellfun inside a class method.
a = ClassdefSiblingHandle(3);
c = ClassdefSiblingHandle(-1);
assert_isequal(viacell(a, c), [true, false]);
%=============================================================================
% Name(obj) where Name is also a class name and obj's class provides an external
% method Name dispatches to that method (converter), not the Name constructor.
s = ClassdefConvSource(9);
assert_isequal(ClassdefConvTarget(s), 509);
%=============================================================================
% Constructing the target class directly (non-source argument) still works.
t = ClassdefConvTarget(4);
assert_isequal(t.w, 4);
%=============================================================================
