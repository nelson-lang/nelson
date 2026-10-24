%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A property set method must not be called when a value-class property is
% initialized to its default (explicit or the implicit []) during construction.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% The property 's' has no default (implicit []); its set method rejects []. The
% object still constructs because the default bypasses the set method.
o = ClassdefSetterGuardedBase();
assert_isequal(o.s, 0);
%=============================================================================
o = ClassdefSetterGuardedBase(5);
assert_isequal(o.s, 5);
%=============================================================================
% The set method is still enforced for genuine assignments.
assert_checkerror('o = ClassdefSetterGuardedBase([1 2 3]);', 's must be real scalar');
%=============================================================================
% Inherited property default in a subclass must also bypass the set method.
d = ClassdefSetterGuardedDerived();
assert_isequal(d.s, 1);
assert_isequal(class(d), 'ClassdefSetterGuardedDerived');
%=============================================================================
d = ClassdefSetterGuardedDerived(7);
assert_isequal(d.s, 7);
%=============================================================================
