%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A subclass whose abstract methods are implemented by external method files
% (@Class/method.m, not declared in the subclass classdef) is concrete and can
% be instantiated.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
o = ClassdefAbstractExternalImpl();
assert_isequal(class(o), 'ClassdefAbstractExternalImpl');
%=============================================================================
% The external instance method satisfies the abstract requirement and dispatches.
assert_isequal(o.instValue(), 7);
%=============================================================================
% The external static method satisfies the abstract static requirement.
assert_isequal(ClassdefAbstractExternalImpl.staticValue(4), 5);
%=============================================================================
% The abstract base itself still cannot be instantiated.
assert_checkerror('ClassdefAbstractExternalBase();', _('Cannot instantiate abstract class: ClassdefAbstractExternalBase'));
%=============================================================================
