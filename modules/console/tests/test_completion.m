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
completionPoint = ClassdefPoint(1, 2);
completionPointResult = completion('completionPoint.');
assert_istrue(completionPointResult.showpopup);
assert_isequal(completionPointResult.prefix, 'completionPoint.');
assert_istrue(any(strcmp(completionPointResult.properties, 'X')));
assert_istrue(any(strcmp(completionPointResult.methods, 'magnitude')));
%=============================================================================
completionPointMethodResult = completion('completionPoint.m');
assert_istrue(completionPointMethodResult.showpopup);
assert_isequal(completionPointMethodResult.prefix, 'm');
assert_istrue(any(strcmp(completionPointMethodResult.methods, 'magnitude')));
%=============================================================================
completionPoints = [ClassdefPoint(3, 4), ClassdefPoint(5, 6)];
completionPointsResult = completion('completionPoints.');
assert_istrue(completionPointsResult.showpopup);
assert_istrue(any(strcmp(completionPointsResult.properties, 'Label')));
assert_istrue(any(strcmp(completionPointsResult.methods, 'shift')));
%=============================================================================
classCompletionResult = completion('ClassdefPoint.');
assert_istrue(classCompletionResult.showpopup);
assert_istrue(any(strcmp(classCompletionResult.properties, 'Dimension')));
assert_istrue(any(strcmp(classCompletionResult.methods, 'origin')));
%=============================================================================
completionCounter = ClassdefCounter(7);
completionCounterResult = completion('completionCounter.');
assert_istrue(completionCounterResult.showpopup);
assert_istrue(any(strcmp(completionCounterResult.properties, 'Count')));
assert_istrue(any(strcmp(completionCounterResult.methods, 'increment')));
assert_istrue(any(strcmp(completionCounterResult.methods, 'zero')));
delete(completionCounter);
%=============================================================================
completionSaveFile = [tempdir(), 'classdef_completion_builtin_restored.mat'];
if isfile(completionSaveFile)
  rmfile(completionSaveFile);
end
completionSavedCounters = [ClassdefCounter(8), ClassdefCounter(9)];
save(completionSaveFile, 'completionSavedCounters');
delete(completionSavedCounters);
clear completionSavedCounters;
clear classes;
load(completionSaveFile);
restoredCounterResult = completion('completionSavedCounters.');
assert_istrue(restoredCounterResult.showpopup);
assert_istrue(any(strcmp(restoredCounterResult.properties, 'Count')));
assert_istrue(any(strcmp(restoredCounterResult.methods, 'increment')));
delete(completionSavedCounters);
%=============================================================================
deletedCounter = ClassdefCounter(10);
delete(deletedCounter);
deletedCounterResult = completion('deletedCounter.');
assert_isfalse(deletedCounterResult.showpopup);
assert_istrue(isempty(deletedCounterResult.properties));
assert_istrue(isempty(deletedCounterResult.methods));
%=============================================================================
rmpath(path_test);
%=============================================================================
