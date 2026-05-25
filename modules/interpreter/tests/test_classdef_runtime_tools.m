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
matVersions = {'-v7', '-v7.3'};
for v = matVersions
  derived = ClassdefDerived(42);
  derived.BaseValue = 14;
  derived = derived.setPrivateProperty(111);
  derived = derived.setReadOnlyProperty(112);
  matFile = [tempdir(), 'classdef_derived_roundtrip', v{1}, '.mat'];
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'derived', v{1});
  clear derived;
  clear classes;
  loadmat(matFile);
  assert_isequal(class(derived), 'ClassdefDerived');
  assert_istrue(isa(derived, 'ClassdefBase'));
  assert_isequal(derived.BaseValue, 14);
  assert_isequal(derived.DerivedValue, 42);
  assert_isequal(derived.readPrivateProperty(), 111);
  assert_isequal(derived.BaseReadOnlyByAccess, 112);
  assert_isequal(derived.derivedMethod(), 56);
end
%=============================================================================
for v = matVersions
  packaged = useClassdefPackagedConstructor(5);
  matFile = [tempdir(), 'classdef_packaged_roundtrip', v{1}, '.mat'];
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'packaged', v{1});
  clear packaged;
  clear classes;
  loadmat(matFile);
  assert_isequal(class(packaged), 'classdefpkg.ClassdefPackaged');
  assert_isequal(packaged.Value, 5);
  assert_isequal(packaged.triple(), 15);
  assert_isequal(useClassdefPackagedConstant(), 12);
end
%=============================================================================
for v = matVersions
  color = useClassdefColorBlue();
  matFile = [tempdir(), 'classdef_enum_roundtrip', v{1}, '.mat'];
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'color', v{1});
  clear color;
  clear classes;
  loadmat(matFile);
  assert_isequal(class(color), 'ClassdefColor');
  assert_isequal(color.Name, 'Blue');
  assert_isfalse(color.isRed());
end
%=============================================================================
for v = matVersions
  counter = ClassdefCounter(8);
  counter.increment(2);
  counter.setSecretCount(123);
  counter.setReadOnlyCount(456);
  counter.WriteOnlyCount = 789;
  counter.LastEvent = 'saved';
  matFile = [tempdir(), 'classdef_handle_roundtrip', v{1}, '.mat'];
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'counter', v{1});
  delete(counter);
  clear counter;
  clear classes;
  loadmat(matFile);
  assert_isequal(class(counter), 'ClassdefCounter');
  assert_istrue(ishandle(counter));
  assert_istrue(isvalid(counter));
  assert_isequal(counter.Count, 10);
  assert_isequal(counter.LastEvent, 'saved');
  assert_isequal(counter.secretCount(), 123);
  assert_isequal(counter.ReadOnlyCount, 456);
  assert_isequal(counter.writeOnlyCount(), 789);
  counter.increment(5);
  assert_isequal(counter.Count, 15);
  delete(counter);
end
%=============================================================================
savedPoint = ClassdefPoint(21, 22);
saveFile = [tempdir(), 'classdef_save_builtin_roundtrip.mat'];
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedPoint');
clear savedPoint;
clear classes;
load(saveFile);
assert_isequal(class(savedPoint), 'ClassdefPoint');
assert_isequal(savedPoint.X, 21);
assert_isequal(savedPoint.Y, 22);
assert_isequal(savedPoint.magnitude(), sqrt(21 * 21 + 22 * 22));
%=============================================================================
savedPoints = [ClassdefPoint(1, 2), ClassdefPoint(3, 4)];
savedPoints(2).X = 100;
savedPoints(1).Label = 'first';
saveFile = [tempdir(), 'classdef_save_builtin_object_array_roundtrip.mat'];
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedPoints');
clear savedPoints;
clear classes;
load(saveFile);
assert_isequal(class(savedPoints), 'ClassdefPoint');
assert_isequal(length(savedPoints), 2);
assert_isequal(savedPoints(1).Label, 'first');
assert_isequal(savedPoints(2).X, 100);
assert_isequal(savedPoints(2).Y, 4);
%=============================================================================
nestedStruct = struct('point', ClassdefPoint(31, 32), ...
  'payload', {{ClassdefDependentValue(9), ClassdefPoint(33, 34)}});
nestedCell = {ClassdefPoint(35, 36), struct('inner', ClassdefDependentValue(10))};
saveFile = [tempdir(), 'classdef_save_builtin_nested_roundtrip.mat'];
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'nestedStruct', 'nestedCell');
clear nestedStruct nestedCell;
clear classes;
load(saveFile);
assert_isequal(class(nestedStruct.point), 'ClassdefPoint');
assert_isequal(nestedStruct.point.X, 31);
assert_isequal(nestedStruct.point.Y, 32);
assert_isequal(class(nestedStruct.payload{1}), 'ClassdefDependentValue');
assert_isequal(nestedStruct.payload{1}.Twice, 18);
assert_isequal(class(nestedStruct.payload{2}), 'ClassdefPoint');
assert_isequal(nestedStruct.payload{2}.Y, 34);
assert_isequal(class(nestedCell{1}), 'ClassdefPoint');
assert_isequal(nestedCell{1}.X, 35);
assert_isequal(class(nestedCell{2}.inner), 'ClassdefDependentValue');
assert_isequal(nestedCell{2}.inner.Twice, 20);
%=============================================================================
nestedStruct = struct('counter', ClassdefCounter(51), ...
  'payload', {{ClassdefPoint(52, 53), ClassdefCounter(54)}});
nestedCell = {ClassdefCounter(55), struct('inner', ClassdefCounter(56))};
saveFile = [tempdir(), 'classdef_save_builtin_nested_handle_roundtrip.mat'];
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'nestedStruct', 'nestedCell');
delete(nestedStruct.counter);
delete(nestedStruct.payload{2});
delete(nestedCell{1});
delete(nestedCell{2}.inner);
clear nestedStruct nestedCell;
clear classes;
load(saveFile);
assert_isequal(class(nestedStruct.counter), 'ClassdefCounter');
assert_isequal(nestedStruct.counter.Count, 51);
assert_isequal(class(nestedStruct.payload{1}), 'ClassdefPoint');
assert_isequal(nestedStruct.payload{1}.Y, 53);
assert_isequal(class(nestedStruct.payload{2}), 'ClassdefCounter');
assert_isequal(nestedStruct.payload{2}.Count, 54);
assert_isequal(class(nestedCell{1}), 'ClassdefCounter');
assert_isequal(nestedCell{1}.Count, 55);
assert_isequal(class(nestedCell{2}.inner), 'ClassdefCounter');
assert_isequal(nestedCell{2}.inner.Count, 56);
delete(nestedStruct.counter);
delete(nestedStruct.payload{2});
delete(nestedCell{1});
delete(nestedCell{2}.inner);
%=============================================================================
for v = matVersions
  savedPoints = [ClassdefPoint(5, 6), ClassdefPoint(7, 8)];
  savedPoints(2).Label = 'second';
  matFile = [tempdir(), 'classdef_object_array_roundtrip', v{1}, '.mat'];
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'savedPoints', v{1});
  clear savedPoints;
  clear classes;
  loadmat(matFile);
  assert_isequal(class(savedPoints), 'ClassdefPoint');
  assert_isequal(length(savedPoints), 2);
  assert_isequal(savedPoints(1).X, 5);
  assert_isequal(savedPoints(2).Y, 8);
  assert_isequal(savedPoints(2).Label, 'second');
end
%=============================================================================
for v = matVersions
  nestedStruct = struct('point', ClassdefPoint(41, 42), ...
    'payload', {{ClassdefDependentValue(11), ClassdefPoint(43, 44)}});
  nestedCell = {ClassdefPoint(45, 46), struct('inner', ClassdefDependentValue(12))};
  matFile = [tempdir(), 'classdef_nested_roundtrip', v{1}, '.mat'];
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'nestedStruct', 'nestedCell', v{1});
  clear nestedStruct nestedCell;
  clear classes;
  loadmat(matFile);
  assert_isequal(class(nestedStruct.point), 'ClassdefPoint');
  assert_isequal(nestedStruct.point.X, 41);
  assert_isequal(nestedStruct.point.Y, 42);
  assert_isequal(class(nestedStruct.payload{1}), 'ClassdefDependentValue');
  assert_isequal(nestedStruct.payload{1}.Twice, 22);
  assert_isequal(class(nestedStruct.payload{2}), 'ClassdefPoint');
  assert_isequal(nestedStruct.payload{2}.Y, 44);
  assert_isequal(class(nestedCell{1}), 'ClassdefPoint');
  assert_isequal(nestedCell{1}.X, 45);
  assert_isequal(class(nestedCell{2}.inner), 'ClassdefDependentValue');
  assert_isequal(nestedCell{2}.inner.Twice, 24);
end
%=============================================================================
for v = matVersions
  nestedStruct = struct('counter', ClassdefCounter(61), ...
    'payload', {{ClassdefPoint(62, 63), ClassdefCounter(64)}});
  nestedCell = {ClassdefCounter(65), struct('inner', ClassdefCounter(66))};
  matFile = [tempdir(), 'classdef_nested_handle_roundtrip', v{1}, '.mat'];
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'nestedStruct', 'nestedCell', v{1});
  delete(nestedStruct.counter);
  delete(nestedStruct.payload{2});
  delete(nestedCell{1});
  delete(nestedCell{2}.inner);
  clear nestedStruct nestedCell;
  clear classes;
  loadmat(matFile);
  assert_isequal(class(nestedStruct.counter), 'ClassdefCounter');
  assert_isequal(nestedStruct.counter.Count, 61);
  assert_isequal(class(nestedStruct.payload{1}), 'ClassdefPoint');
  assert_isequal(nestedStruct.payload{1}.Y, 63);
  assert_isequal(class(nestedStruct.payload{2}), 'ClassdefCounter');
  assert_isequal(nestedStruct.payload{2}.Count, 64);
  assert_isequal(class(nestedCell{1}), 'ClassdefCounter');
  assert_isequal(nestedCell{1}.Count, 65);
  assert_isequal(class(nestedCell{2}.inner), 'ClassdefCounter');
  assert_isequal(nestedCell{2}.inner.Count, 66);
  delete(nestedStruct.counter);
  delete(nestedStruct.payload{2});
  delete(nestedCell{1});
  delete(nestedCell{2}.inner);
end
%=============================================================================
savedDependent = ClassdefDependentValue(12);
saveFile = [tempdir(), 'classdef_dependent_value_roundtrip.mat'];
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedDependent');
clear savedDependent;
clear classes;
load(saveFile);
assert_isequal(class(savedDependent), 'ClassdefDependentValue');
assert_isequal(savedDependent.Base, 12);
assert_isequal(savedDependent.Twice, 24);
%=============================================================================
for v = matVersions
  savedDependent = ClassdefDependentValue(13);
  matFile = [tempdir(), 'classdef_dependent_value_roundtrip', v{1}, '.mat'];
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'savedDependent', v{1});
  clear savedDependent;
  clear classes;
  loadmat(matFile);
  assert_isequal(class(savedDependent), 'ClassdefDependentValue');
  assert_isequal(savedDependent.Base, 13);
  assert_isequal(savedDependent.Twice, 26);
end
%=============================================================================
for v = matVersions
  savedDependentHandle = ClassdefDependentHandle(14);
  matFile = [tempdir(), 'classdef_dependent_handle_roundtrip', v{1}, '.mat'];
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'savedDependentHandle', v{1});
  delete(savedDependentHandle);
  clear savedDependentHandle;
  clear classes;
  loadmat(matFile);
  assert_isequal(class(savedDependentHandle), 'ClassdefDependentHandle');
  assert_isequal(savedDependentHandle.Base, 14);
  assert_isequal(savedDependentHandle.Twice, 28);
  delete(savedDependentHandle);
end
%=============================================================================
savedCounter = ClassdefCounter(13);
savedCounter.increment(2);
savedCounter.setSecretCount(321);
savedCounter.setReadOnlyCount(654);
savedCounter.WriteOnlyCount = 987;
saveFile = [tempdir(), 'classdef_save_builtin_handle_roundtrip.mat'];
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedCounter');
delete(savedCounter);
clear savedCounter;
clear classes;
load(saveFile);
assert_isequal(class(savedCounter), 'ClassdefCounter');
assert_isequal(savedCounter.Count, 15);
assert_isequal(savedCounter.secretCount(), 321);
assert_isequal(savedCounter.ReadOnlyCount, 654);
assert_isequal(savedCounter.writeOnlyCount(), 987);
savedCounter.increment(1);
assert_isequal(savedCounter.Count, 16);
restoredCounterProperties = properties(savedCounter);
restoredCounterMethods = methods(savedCounter);
restoredCounterEvents = events(savedCounter);
restoredCounterFields = fieldnames(savedCounter);
assert_istrue(any(strcmp(restoredCounterProperties, 'Count')));
assert_istrue(any(strcmp(restoredCounterMethods, 'increment')));
assert_istrue(any(strcmp(restoredCounterEvents, 'CountChanged')));
assert_istrue(any(strcmp(restoredCounterFields, 'Count')));
assert_istrue(isprop(savedCounter, 'Count'));
dbclear all;
dbstop in ClassdefCounter at 38;
restoredCounterBreakpoints = dbstatus();
assert_isequal(restoredCounterBreakpoints.name, 'ClassdefCounter');
assert_istrue(endsWith(restoredCounterBreakpoints.file, 'ClassdefCounter.m'));
assert_isequal(restoredCounterBreakpoints.line, 38);
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
assert_istrue(any(strcmp(restoredCounterProfileFiles, restoredCounterFile) & restoredCounterProfileLines == 38));
assert_isequal(savedCounter.Count, 18);
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
  if strcmp(profInfo(k).Filename, classdefPointFile) && profInfo(k).LinePosition == 19
    foundPointConstructor = true;
  end
  if strcmp(profInfo(k).Filename, classdefPointFile) && profInfo(k).LinePosition == 29
    foundPointMethod = true;
  end
  if strcmp(profInfo(k).Filename, classdefCounterFile) && profInfo(k).LinePosition == 30
    foundCounterConstructor = true;
  end
  if strcmp(profInfo(k).Filename, classdefCounterFile) && profInfo(k).LinePosition == 38
    foundCounterMethod = true;
  end
end
assert_istrue(foundPointConstructor);
assert_istrue(foundPointMethod);
assert_istrue(foundCounterConstructor);
assert_istrue(foundCounterMethod);
delete(counterForProfile);
profile('clear');
%=============================================================================
dbclear all;
dbstop in ClassdefPoint at 19;
dbstop in ClassdefCounter at 38;
bp = dbstatus();
assert_isequal(length(bp), 2);
assert_istrue(any(strcmp({bp.name}, 'ClassdefPoint')));
assert_istrue(any(strcmp({bp.name}, 'ClassdefCounter')));
assert_istrue(any([bp.line] == 19));
assert_istrue(any([bp.line] == 38));
dbclear all;
%=============================================================================
completionPoint = ClassdefPoint(1, 2);
completionPoints = [ClassdefPoint(1, 2), ClassdefPoint(3, 4)];
completionCounter = ClassdefCounter(1);
completionCounters = [ClassdefCounter(2), ClassdefCounter(3)];
assert_istrue(any(strcmp(properties(completionPoint), 'X')));
assert_istrue(any(strcmp(methods(completionPoint), 'magnitude')));
assert_istrue(any(strcmp(properties(completionPoints), 'X')));
assert_istrue(any(strcmp(methods(completionPoints), 'magnitude')));
assert_isequal(isprop(completionPoints, 'X'), [true, true]);
assert_istrue(any(strcmp(properties('ClassdefPoint'), 'Dimension')));
assert_istrue(any(strcmp(methods('ClassdefPoint'), 'origin')));
assert_istrue(any(strcmp(properties(completionCounter), 'Count')));
assert_istrue(any(strcmp(methods(completionCounter), 'increment')));
assert_istrue(any(strcmp(properties(completionCounters), 'Count')));
assert_istrue(any(strcmp(methods(completionCounters), 'increment')));
assert_istrue(any(strcmp(fieldnames(completionCounters), 'Count')));
assert_istrue(any(strcmp(events(completionCounters), 'CountChanged')));
assert_isequal(isprop(completionCounters, 'Count'), [true, true]);
completionSaveFile = [tempdir(), 'classdef_completion_restored_arrays.mat'];
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
assert_isequal(class(completionSavedPoints), 'ClassdefPoint');
assert_isequal(length(completionSavedPoints), 2);
assert_istrue(any(strcmp(properties(completionSavedPoints), 'X')));
assert_istrue(any(strcmp(methods(completionSavedPoints), 'magnitude')));
assert_isequal(isprop(completionSavedPoints, 'Label'), [true, true]);
assert_isequal(class(completionSavedCounters), 'ClassdefCounter');
assert_isequal(length(completionSavedCounters), 2);
assert_istrue(any(strcmp(properties(completionSavedCounters), 'Count')));
assert_istrue(any(strcmp(methods(completionSavedCounters), 'increment')));
assert_istrue(any(strcmp(fieldnames(completionSavedCounters), 'Count')));
assert_istrue(any(strcmp(events(completionSavedCounters), 'CountChanged')));
assert_isequal(isprop(completionSavedCounters, 'LastEvent'), [true, true]);
delete(completionSavedCounters);
delete(completionCounters);
delete(completionCounter);
assert_checkerror('properties(completionCounter);', 'Invalid handle.');
assert_checkerror('methods(completionCounter);', 'Invalid handle.');
%=============================================================================
rmpath(path_test);
%=============================================================================
