%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression test: builtin('subsref', obj, S) and builtin('subsasgn', obj, S,
% val) must perform the DEFAULT structural indexing and must NOT re-dispatch to
% the object's own overloaded subsref/subsasgn. A class whose overloaded
% subsref/subsasgn uses builtin(...) as its fallback (as Chebfun's chebpref /
% cheboppref do) would otherwise recurse forever ("Allowable stack depth
% exceeded"). builtin() executes the built-in version, ignoring the
% overload.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
b = ClassdefBuiltinSubsref();
%=============================================================================
% Fallback branch: a real property is read through builtin('subsref', ...).
assert_isequal(b.name, 'default');
%=============================================================================
% isfield branch: a preference in prefList is served directly (no builtin call).
assert_isequal(b.vectorize, true);
assert_isequal(b.maxLength, 65);
%=============================================================================
% Fallback branch: a real property is written through builtin('subsasgn', ...).
b.name = 'changed';
assert_isequal(b.name, 'changed');
%=============================================================================
% isfield branch: writing a preference goes directly to prefList.
b.vectorize = false;
assert_isequal(b.vectorize, false);
% the real property is untouched by the preference write
assert_isequal(b.name, 'changed');
%=============================================================================
% Reading a missing property flows to builtin('subsref', ...) which must raise
% "No such property" instead of recursing into the overloaded subsref.
assert_checkerror('r = b.nosuchthing;', _('No such property: nosuchthing'));
%=============================================================================
% Writing a missing property flows to builtin('subsasgn', ...) which must raise
% "No such property" instead of recursing into the overloaded subsasgn.
assert_checkerror('b.nosuchthing = 42;', _('No such property: nosuchthing'));
%=============================================================================
% Value semantics: a copy taken before a fallback write is not mutated.
b1 = ClassdefBuiltinSubsref();
b2 = b1;
b1.name = 'mutated';
assert_isequal(b1.name, 'mutated');
assert_isequal(b2.name, 'default');
%=============================================================================
