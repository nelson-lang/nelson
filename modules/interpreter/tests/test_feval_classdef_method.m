%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% feval(obj, args) dispatches to the object's feval method, resolved along the
% inheritance chain, exactly like obj.feval(args).
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
o = ClassdefFevalDerived();
%=============================================================================
% Function form and dot form agree.
assert_isequal(feval(o, 5), 15);
assert_isequal(o.feval(5), 15);
%=============================================================================
% feval on a plain function handle and name still works.
assert_isequal(feval(@sin, 0), 0);
assert_isequal(feval('cos', 0), 1);
assert_isequal(feval(@(x) x * x, 4), 16);
%=============================================================================
