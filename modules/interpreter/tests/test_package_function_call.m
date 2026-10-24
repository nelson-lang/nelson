%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Package functions are called from packageCallHelper(), which is compiled
% when it is invoked, i.e. after the package folder is on the path. This
% mirrors real usage where a module (and its package) is loaded before any
% caller is compiled.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/package_function_call'];
addpath(path_test, '-begin');
clear functions
%=============================================================================
% This file is compiled before its own addpath() runs, so an inline
% single-level call is compiled while +Pkg is not yet on the path. It is
% resolved at run time by the CALL_CLASSDEF_MEMBER fallback.
runtimeFallback = Pkg.greet();
asserts.isequal(runtimeFallback, 'pkg-greet');
%=============================================================================
[singleLevel, nested, deepest, coexistStatic, coexistPackage] = packageCallHelper();
%=============================================================================
% Direct call of a single-level package function (+Pkg/greet.m).
asserts.isequal(singleLevel, 'pkg-greet');
%=============================================================================
% Direct call of a nested package function (+Pkg/+util/inner.m).
asserts.isequal(nested, 'pkg-util-inner');
%=============================================================================
% Direct call of a deeply nested package function (+Pkg/+util/+more/deepest.m).
asserts.isequal(deepest, 'pkg-deepest');
%=============================================================================
% A classdef static method and a package function share the top-level name
% 'Thing'. The static method wins when it exists.
asserts.isequal(coexistStatic, 'thing-static-make');
%=============================================================================
% The package function is used when the classdef has no matching static member.
asserts.isequal(coexistPackage, 'thing-pkg-build');
%=============================================================================
% feval keeps resolving the same qualified names.
asserts.isequal(feval('Pkg.greet'), 'pkg-greet');
asserts.isequal(feval('Pkg.util.inner'), 'pkg-util-inner');
%=============================================================================
% An unknown qualified member still raises an error.
asserts.checkerror('r = Pkg.missing();', 'Undefined classdef member: Pkg.missing');
%=============================================================================
rmpath(path_test);
%=============================================================================
