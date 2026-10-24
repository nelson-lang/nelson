%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A package member referenced without parentheses (e.g. `Pkg.util.inner`)
% must resolve like the parenthesised call `Pkg.util.inner()`, at any package
% nesting depth. Regression guard for the "Undefined variable: <head>" bug on
% bare (no-parenthesis) nested package member references.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/package_function_call'];
addpath(path_test, '-begin');
clear functions
%=============================================================================
% Compile-time path (function body): packageBareRefHelper is compiled after
% +Pkg is on the path, so each bare reference lowers to a direct dotted call.
[barePkgFn, bareNestedFn, bareDeepFn, bareStatic] = packageBareRefHelper();
assert_isequal(barePkgFn, 'pkg-greet');
assert_isequal(bareNestedFn, 'pkg-util-inner');
assert_isequal(bareDeepFn, 'pkg-util-deep-deephelper');
assert_isequal(bareStatic, 'pkg-util-gadget-spin');
%=============================================================================
% Run-time path (script/immediate): these call sites are compiled before the
% addpath above runs, so they are resolved by the run-time member handler.
% Single-level package function without parentheses.
r = Pkg.greet;
assert_isequal(r, 'pkg-greet');
% Two-level nested package function without parentheses.
r = Pkg.util.inner;
assert_isequal(r, 'pkg-util-inner');
% Three-level nested package function without parentheses.
r = Pkg.util.deep.deephelper;
assert_isequal(r, 'pkg-util-deep-deephelper');
% Static method of a classdef living inside a nested package, without
% parentheses.
r = Pkg.util.Gadget.spin;
assert_isequal(r, 'pkg-util-gadget-spin');
%=============================================================================
% Bare reference used directly as a call argument.
assert_isequal(upper(Pkg.greet), 'PKG-GREET');
%=============================================================================
% Regression: a plain struct field chain (head is a real variable) keeps its
% ordinary field-access semantics and is not mistaken for a package member.
s.a.b = 99;
assert_isequal(s.a.b, 99);
%=============================================================================
% An unknown qualified member without parentheses still raises an error.
assert_checkerror('r = Pkg.nope.missing;', 'Undefined classdef member: Pkg.nope.missing');
%=============================================================================
rmpath(path_test);
%=============================================================================
