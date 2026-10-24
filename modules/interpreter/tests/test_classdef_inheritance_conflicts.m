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
asserts.isequal(parsefile([path_test, '/ClassdefConflictPropertyChild.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefConflictMethodChild.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefDuplicateProperty.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefDuplicateMethod.m']), 'classdef');
%=============================================================================
asserts.checkerror('ClassdefDuplicateProperty()', 'Duplicate property: Value');
asserts.checkerror('ClassdefDuplicateMethod()', 'Duplicate method: repeated');
asserts.checkerror('ClassdefConflictPropertyChild()', ...
  'Inheritance property conflict: SharedValue');
asserts.checkerror('ClassdefConflictMethodChild()', ...
  'Inheritance method conflict: sharedMethod');
%=============================================================================
obj = ClassdefConflictMethodResolved();
asserts.isequal(obj.sharedMethod(), 3);
methodNames = methods('ClassdefConflictMethodResolved');
asserts.istrue(any(strcmp(methodNames, 'sharedMethod')));
%=============================================================================
rmpath(path_test);
%=============================================================================
