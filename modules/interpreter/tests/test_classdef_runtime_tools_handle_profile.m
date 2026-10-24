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
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
%=============================================================================
savedCounter = ClassdefCounter(13);
savedCounter.increment(2);
savedCounter.setSecretCount(321);
savedCounter.setReadOnlyCount(654);
savedCounter.WriteOnlyCount = 987;
saveFile = fullfile(fixtureDirectory, 'classdef_save_builtin_handle_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedCounter');
delete(savedCounter);
clear savedCounter;
clear classes;
load(saveFile);
asserts.isequal(class(savedCounter), 'ClassdefCounter');
asserts.isequal(savedCounter.Count, 15);
asserts.isequal(savedCounter.secretCount(), 321);
asserts.isequal(savedCounter.ReadOnlyCount, 654);
asserts.isequal(savedCounter.writeOnlyCount(), 987);
savedCounter.increment(1);
asserts.isequal(savedCounter.Count, 16);
restoredCounterProperties = properties(savedCounter);
restoredCounterMethods = methods(savedCounter);
restoredCounterEvents = events(savedCounter);
restoredCounterFields = fieldnames(savedCounter);
asserts.istrue(any(strcmp(restoredCounterProperties, 'Count')));
asserts.istrue(any(strcmp(restoredCounterMethods, 'increment')));
asserts.istrue(any(strcmp(restoredCounterEvents, 'CountChanged')));
asserts.istrue(any(strcmp(restoredCounterFields, 'Count')));
asserts.istrue(isprop(savedCounter, 'Count'));
dbclear all;
dbstop in ClassdefCounter at 38;
restoredCounterBreakpoints = dbstatus();
asserts.isequal(restoredCounterBreakpoints.name, 'ClassdefCounter');
asserts.istrue(endsWith(restoredCounterBreakpoints.file, 'ClassdefCounter.m'));
asserts.isequal(restoredCounterBreakpoints.line, 39);
dbclear all;
profile('clear');
profile('on');
savedCounter.increment(2);
profile('off');
restoredCounterProfile = profile('info', 'line');
profile('clear');
restoredCounterProfileFiles = {restoredCounterProfile.Filename};
restoredCounterProfileLines = [restoredCounterProfile.LinePosition];
restoredCounterFile = [path_test, '/ClassdefCounter.m'];
asserts.istrue(any(strcmp(restoredCounterProfileFiles, restoredCounterFile) & ...
  (restoredCounterProfileLines == 38 | restoredCounterProfileLines == 39)));
asserts.isequal(savedCounter.Count, 18);
delete(savedCounter);
%=============================================================================
profile('clear');
profile('on');
pointForProfile = useClassdefPointStatic();
pointForProfile = pointForProfile.shift(2, 3);
counterForProfile = useClassdefCounterStatic();
counterForProfile.increment(4);
profile('off');
profInfo = profile('info');
classdefPointFile = [path_test, '/ClassdefPoint.m'];
classdefCounterFile = [path_test, '/ClassdefCounter.m'];
foundPointConstructor = false;
foundPointMethod = false;
foundCounterConstructor = false;
foundCounterMethod = false;
for k = 1:length(profInfo)
  if strcmp(profInfo(k).Filename, classdefPointFile) && profInfo(k).LinePosition == 15
    foundPointConstructor = true;
  end
  if strcmp(profInfo(k).Filename, classdefPointFile) && profInfo(k).LinePosition == 25
    foundPointMethod = true;
  end
  if strcmp(profInfo(k).Filename, classdefCounterFile) && ...
      (profInfo(k).LinePosition == 32 || profInfo(k).LinePosition == 33)
    foundCounterConstructor = true;
  end
  if strcmp(profInfo(k).Filename, classdefCounterFile) && ...
      (profInfo(k).LinePosition == 38 || profInfo(k).LinePosition == 39)
    foundCounterMethod = true;
  end
end
asserts.istrue(foundPointConstructor);
asserts.istrue(foundPointMethod);
asserts.istrue(foundCounterConstructor);
asserts.istrue(foundCounterMethod);
delete(counterForProfile);
profile('clear');
%=============================================================================
dbclear all;
dbstop in ClassdefPoint at 15;
dbstop in ClassdefCounter at 38;
bp = dbstatus();
asserts.isequal(length(bp), 2);
asserts.istrue(any(strcmp({bp.name}, 'ClassdefPoint')));
asserts.istrue(any(strcmp({bp.name}, 'ClassdefCounter')));
asserts.istrue(any([bp.line] == 15));
asserts.istrue(any([bp.line] == 39));
dbclear all;
%=============================================================================
completionPoint = ClassdefPoint(1, 2);
completionPoints = [ClassdefPoint(1, 2), ClassdefPoint(3, 4)];
completionCounter = ClassdefCounter(1);
completionCounters = [ClassdefCounter(2), ClassdefCounter(3)];
asserts.istrue(any(strcmp(properties(completionPoint), 'X')));
asserts.istrue(any(strcmp(methods(completionPoint), 'magnitude')));
asserts.istrue(any(strcmp(properties(completionPoints), 'X')));
asserts.istrue(any(strcmp(methods(completionPoints), 'magnitude')));
asserts.isequal(isprop(completionPoints, 'X'), [true, true]);
asserts.istrue(any(strcmp(properties('ClassdefPoint'), 'Dimension')));
asserts.istrue(any(strcmp(methods('ClassdefPoint'), 'origin')));
asserts.istrue(any(strcmp(properties(completionCounter), 'Count')));
asserts.istrue(any(strcmp(methods(completionCounter), 'increment')));
asserts.istrue(any(strcmp(properties(completionCounters), 'Count')));
asserts.istrue(any(strcmp(methods(completionCounters), 'increment')));
asserts.istrue(any(strcmp(fieldnames(completionCounters), 'Count')));
asserts.istrue(any(strcmp(events(completionCounters), 'CountChanged')));
asserts.isequal(isprop(completionCounters, 'Count'), [true, true]);
completionSaveFile = fullfile(fixtureDirectory, 'classdef_completion_restored_arrays.mat');
if isfile(completionSaveFile)
  rmfile(completionSaveFile);
end
completionSavedPoints = [ClassdefPoint(10, 11), ClassdefPoint(12, 13)];
completionSavedCounters = [ClassdefCounter(14), ClassdefCounter(15)];
save(completionSaveFile, 'completionSavedPoints', 'completionSavedCounters');
delete(completionSavedCounters);
clear completionSavedPoints completionSavedCounters;
clear classes;
load(completionSaveFile);
asserts.isequal(class(completionSavedPoints), 'ClassdefPoint');
asserts.isequal(length(completionSavedPoints), 2);
asserts.istrue(any(strcmp(properties(completionSavedPoints), 'X')));
asserts.istrue(any(strcmp(methods(completionSavedPoints), 'magnitude')));
asserts.isequal(isprop(completionSavedPoints, 'Label'), [true, true]);
asserts.isequal(class(completionSavedCounters), 'ClassdefCounter');
asserts.isequal(length(completionSavedCounters), 2);
asserts.istrue(any(strcmp(properties(completionSavedCounters), 'Count')));
asserts.istrue(any(strcmp(methods(completionSavedCounters), 'increment')));
asserts.istrue(any(strcmp(fieldnames(completionSavedCounters), 'Count')));
asserts.istrue(any(strcmp(events(completionSavedCounters), 'CountChanged')));
asserts.isequal(isprop(completionSavedCounters, 'LastEvent'), [true, true]);
delete(completionSavedCounters);
invalidCompletionCounter = ClassdefCounter(99);
delete(invalidCompletionCounter);
asserts.checkerror('properties(invalidCompletionCounter);', _('Invalid handle.'));
asserts.checkerror('methods(invalidCompletionCounter);', _('Invalid handle.'));
%=============================================================================
rmpath(path_test);
rmdir(fixtureDirectory, 's');
%=============================================================================
