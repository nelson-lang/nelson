%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.istrue(isbuiltin('asserts.fail'));
asserts.istrue(isbuiltin('asserts.noError'));
asserts.istrue(isbuiltin('asserts.warning'));
asserts.istrue(isbuiltin('asserts.notEqual'));
asserts.istrue(isbuiltin('asserts.notApprox'));
asserts.istrue(isbuiltin('asserts.lessThan'));
asserts.istrue(isbuiltin('asserts.lessOrEqual'));
asserts.istrue(isbuiltin('asserts.greaterThan'));
asserts.istrue(isbuiltin('asserts.greaterOrEqual'));
asserts.istrue(isbuiltin('asserts.inRange'));
asserts.istrue(isbuiltin('asserts.rowVector'));
asserts.istrue(isbuiltin('asserts.columnVector'));
asserts.istrue(isbuiltin('asserts.squareMatrix'));
asserts.istrue(isbuiltin('asserts.ndims'));
asserts.istrue(isbuiltin('asserts.length'));
asserts.istrue(isbuiltin('asserts.rows'));
asserts.istrue(isbuiltin('asserts.columns'));
asserts.istrue(isbuiltin('asserts.startsWith'));
asserts.istrue(isbuiltin('asserts.endsWith'));
asserts.istrue(isbuiltin('asserts.containsAll'));
asserts.istrue(isbuiltin('asserts.containsAny'));
asserts.istrue(isbuiltin('asserts.matchesAll'));
asserts.istrue(isbuiltin('asserts.matchesAny'));
asserts.istrue(isbuiltin('asserts.hasField'));
asserts.istrue(isbuiltin('asserts.hasFields'));
asserts.istrue(isbuiltin('asserts.fields'));
asserts.istrue(isbuiltin('asserts.satisfies'));
asserts.isequal(which('asserts.checkwarning'), '');
asserts.isequal(which('asserts.checkerror_contains'), '');
%=============================================================================
% Completion entries are replacements for the leaf after the typed container.
assertsCompletion = completion('asserts.');
asserts.istrue(assertsCompletion.showpopup);
asserts.istrue(any(strcmp(assertsCompletion.builtin, 'fields')));
asserts.istrue(any(strcmp(assertsCompletion.builtin, 'istrue')));
asserts.isfalse(any(strcmp(assertsCompletion.builtin, 'checkwarning')));
asserts.isfalse(any(strcmp(assertsCompletion.builtin, 'checkerror_contains')));
%=============================================================================
assertsFilteredCompletion = completion('asserts.f');
asserts.istrue(assertsFilteredCompletion.showpopup);
asserts.isequal(assertsFilteredCompletion.prefix, 'f');
asserts.istrue(any(strcmp(assertsFilteredCompletion.builtin, 'fail')));
asserts.istrue(any(strcmp(assertsFilteredCompletion.builtin, 'fields')));
asserts.istrue(any(strcmp(assertsFilteredCompletion.builtin, 'finite')));
%=============================================================================
