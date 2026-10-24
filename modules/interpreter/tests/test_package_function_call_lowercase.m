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
% Top-level package-qualified calls with a lowercase package name. This file
% is compiled before its own addpath() runs, so every inline call below is
% compiled while +lowerpkg is not yet on the path: the calls are resolved at
% run time. A top-level Name.member(...) is lowered to the runtime
% classdef-member dispatch (the same path the uppercase package test uses), so
% lowercase and uppercase package names behave identically.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/package_function_call'];
addpath(path_test, '-begin');
clear functions
%=============================================================================
% Direct call of a single-level package function (+lowerpkg/twice.m).
asserts.isequal(lowerpkg.twice(21), 42);
%=============================================================================
% Varargout package wrappers must honor zero-output calls.
lowerpkg.varwrap(9);
asserts.isequal(lowerpkg.varwrap(9), 18);
lowerpkg.wrapclear('temporary_variable_that_does_not_exist');
%=============================================================================
% Call in expression context.
r = lowerpkg.twice(10) + 1;
asserts.isequal(r, 21);
%=============================================================================
% Call in argument position.
asserts.isequal(sprintf('%d', lowerpkg.twice(4)), '8');
%=============================================================================
% Multiple outputs.
[a, b] = lowerpkg.pair(7);
asserts.isequal(a, 8);
asserts.isequal(b, 6);
%=============================================================================
% Nested package function (+lowerpkg/+inner/triple.m).
asserts.isequal(lowerpkg.inner.triple(5), 15);
%=============================================================================
% Anonymous function body.
f = @(x) lowerpkg.twice(x) + 100;
asserts.isequal(f(1), 102);
%=============================================================================
% exist() keeps reporting the package function.
asserts.isequal(exist('lowerpkg.twice'), 2);
%=============================================================================
% A variable takes precedence over the package of the same name.
lowerpkg = struct('twice', [10 20 30]);
asserts.isequal(lowerpkg.twice(2), 20);
clear lowerpkg
%=============================================================================
% import + unqualified call keeps working.
import lowerpkg.*
asserts.isequal(twice(3), 6);
%=============================================================================
% A dotted call whose head resolves to nothing at run time reports the
% qualified member, matching the uppercase package path.
asserts.checkerror('r = zzz_undefined_head.field(1);', 'Undefined classdef member: zzz_undefined_head.field');
%=============================================================================
% An unknown member of a real package errors too.
asserts.checkerror('r = lowerpkg.missing_member(1);', 'Undefined classdef member: lowerpkg.missing_member');
%=============================================================================
rmpath(path_test);
%=============================================================================
