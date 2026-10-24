%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A static method reached through an instance (obj.staticMethod(args)) is
% dispatched to the static method, without passing the object as an argument.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
o = ClassdefStaticViaInstance(7);
%=============================================================================
% Static method through the instance: the object is not passed.
assert_isequal(o.combine(3, 4), 34);
%=============================================================================
% Same static method through the class name.
assert_isequal(ClassdefStaticViaInstance.combine(3, 4), 34);
%=============================================================================
% Instance methods through the instance still receive the object.
assert_isequal(o.addInstance(5), 12);
%=============================================================================
