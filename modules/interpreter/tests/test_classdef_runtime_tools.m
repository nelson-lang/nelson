%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
%=============================================================================
matVersions = {'-v7', '-v7.3'};
for v = matVersions
  derived = ClassdefDerived(42);
  derived.BaseValue = 14;
  derived = derived.setPrivateProperty(111);
  derived = derived.setReadOnlyProperty(112);
  matFile = fullfile(fixtureDirectory, ['classdef_derived_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'derived', v{1});
  clear derived;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(derived), 'ClassdefDerived');
  asserts.istrue(isa(derived, 'ClassdefBase'));
  asserts.isequal(derived.BaseValue, 14);
  asserts.isequal(derived.DerivedValue, 42);
  asserts.isequal(derived.readPrivateProperty(), 111);
  asserts.isequal(derived.BaseReadOnlyByAccess, 112);
  asserts.isequal(derived.derivedMethod(), 56);
end
%=============================================================================
for v = matVersions
  packaged = useClassdefPackagedConstructor(5);
  matFile = fullfile(fixtureDirectory, ['classdef_packaged_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'packaged', v{1});
  clear packaged;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(packaged), 'classdefpkg.ClassdefPackaged');
  asserts.isequal(packaged.Value, 5);
  asserts.isequal(packaged.triple(), 15);
  asserts.isequal(useClassdefPackagedConstant(), 12);
end
%=============================================================================
for v = matVersions
  color = useClassdefColorBlue();
  matFile = fullfile(fixtureDirectory, ['classdef_enum_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'color', v{1});
  clear color;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(color), 'ClassdefColor');
  asserts.isequal(color.Name, 'Blue');
  asserts.isfalse(color.isRed());
end
%=============================================================================
for v = matVersions
  counter = ClassdefCounter(8);
  counter.increment(2);
  counter.setSecretCount(123);
  counter.setReadOnlyCount(456);
  counter.WriteOnlyCount = 789;
  counter.LastEvent = 'saved';
  matFile = fullfile(fixtureDirectory, ['classdef_handle_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'counter', v{1});
  delete(counter);
  clear counter;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(counter), 'ClassdefCounter');
  asserts.istrue(ishandle(counter));
  asserts.istrue(isvalid(counter));
  asserts.isequal(counter.Count, 10);
  asserts.isequal(counter.LastEvent, 'saved');
  asserts.isequal(counter.secretCount(), 123);
  asserts.isequal(counter.ReadOnlyCount, 456);
  asserts.isequal(counter.writeOnlyCount(), 789);
  counter.increment(5);
  asserts.isequal(counter.Count, 15);
  delete(counter);
end
%=============================================================================
savedPoint = ClassdefPoint(21, 22);
saveFile = fullfile(fixtureDirectory, 'classdef_save_builtin_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedPoint');
clear savedPoint;
clear classes;
load(saveFile);
asserts.isequal(class(savedPoint), 'ClassdefPoint');
asserts.isequal(savedPoint.X, 21);
asserts.isequal(savedPoint.Y, 22);
asserts.isequal(savedPoint.Dimension, 2);
asserts.isequal(savedPoint.magnitude(), sqrt(21 * 21 + 22 * 22));
savedPointProperties = properties(savedPoint);
savedPointMethods = methods(savedPoint);
savedPointMeta = metaclass(savedPoint);
savedPointStruct = struct(savedPoint);
asserts.istrue(any(strcmp(savedPointProperties, 'X')));
asserts.istrue(any(strcmp(savedPointProperties, 'Dimension')));
asserts.istrue(any(strcmp(savedPointMethods, 'magnitude')));
asserts.isequal(savedPointMeta.Name, 'ClassdefPoint');
asserts.isequal(fieldnames(savedPointStruct), {'X'; 'Y'; 'Label'});
asserts.isfalse(any(strcmp(fieldnames(savedPointStruct), 'Dimension')));
%=============================================================================
hookObject = ClassdefSaveLoadHooks(23, 'runtime');
hookData = hookObject.saveObjectImpl();
asserts.isequal(hookData.Value, 123);
asserts.isequal(hookData.Label, 'runtime_saved');
asserts.isequal(hookData.Twice, 46);
hookRestored = ClassdefSaveLoadHooks.loadObjectImpl(hookData);
asserts.isequal(class(hookRestored), 'ClassdefSaveLoadHooks');
asserts.isequal(hookRestored.Value, 33);
asserts.isequal(hookRestored.Label, 'runtime_saved_loaded');
asserts.isequal(hookRestored.Twice, 66);
hookMethods = methods(hookRestored);
asserts.istrue(any(strcmp(hookMethods, 'saveObjectImpl')));
asserts.istrue(any(strcmp(hookMethods, 'loadObjectImpl')));
saveFile = fullfile(fixtureDirectory, 'classdef_save_load_hooks_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'hookObject');
clear hookObject;
clear classes;
load(saveFile);
asserts.isequal(class(hookObject), 'ClassdefSaveLoadHooks');
asserts.isequal(hookObject.Value, 33);
asserts.isequal(hookObject.Label, 'runtime_saved_loaded');
asserts.isequal(hookObject.Twice, 66);
asserts.isequal(fieldnames(struct(hookObject)), {'Value'; 'Label'});
handleHookObject = ClassdefHandleSaveLoadHooks(5);
saveFile = fullfile(fixtureDirectory, 'classdef_handle_save_load_hooks_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'handleHookObject');
delete(handleHookObject);
clear handleHookObject;
clear classes;
load(saveFile);
asserts.isequal(class(handleHookObject), 'ClassdefHandleSaveLoadHooks');
asserts.istrue(isvalid(handleHookObject));
asserts.isequal(handleHookObject.Value, 35);
delete(handleHookObject);
clear handleHookObject;
global CLASSDEF_HANDLE_SAVE_HOOK_COUNT;
CLASSDEF_HANDLE_SAVE_HOOK_COUNT = 0;
partialHandleHookArray = [ClassdefHandleSaveLoadHooks(7), ClassdefHandleSaveLoadHooks(8)];
delete(partialHandleHookArray(2));
asserts.checkerror('save(saveFile, ''partialHandleHookArray'');', _('Invalid handle.'));
asserts.isequal(CLASSDEF_HANDLE_SAVE_HOOK_COUNT, 0);
delete(partialHandleHookArray(1));
clear partialHandleHookArray CLASSDEF_HANDLE_SAVE_HOOK_COUNT;
%=============================================================================
nestedHookStruct = struct('inner', ClassdefSaveLoadHooks(30, 'nestedStruct'), ...
  'payload', {{ClassdefSaveLoadHooks(31, 'nestedCell')}});
nestedHookCell = {ClassdefSaveLoadHooks(32, 'cellValue'), ...
  struct('inner', ClassdefSaveLoadHooks(33, 'cellStruct'))};
saveFile = fullfile(fixtureDirectory, 'classdef_save_load_hooks_nested_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'nestedHookStruct', 'nestedHookCell');
clear nestedHookStruct nestedHookCell;
clear classes;
load(saveFile);
asserts.isequal(class(nestedHookStruct.inner), 'ClassdefSaveLoadHooks');
asserts.isequal(nestedHookStruct.inner.Value, 40);
asserts.isequal(nestedHookStruct.inner.Label, 'nestedStruct_saved_loaded');
asserts.isequal(nestedHookStruct.inner.Twice, 80);
asserts.isequal(class(nestedHookStruct.payload{1}), 'ClassdefSaveLoadHooks');
asserts.isequal(nestedHookStruct.payload{1}.Value, 41);
asserts.isequal(nestedHookStruct.payload{1}.Label, 'nestedCell_saved_loaded');
asserts.isequal(class(nestedHookCell{1}), 'ClassdefSaveLoadHooks');
asserts.isequal(nestedHookCell{1}.Value, 42);
asserts.isequal(nestedHookCell{1}.Label, 'cellValue_saved_loaded');
asserts.isequal(class(nestedHookCell{2}.inner), 'ClassdefSaveLoadHooks');
asserts.isequal(nestedHookCell{2}.inner.Value, 43);
asserts.isequal(nestedHookCell{2}.inner.Label, 'cellStruct_saved_loaded');
hookArray = [ClassdefSaveLoadHooks(34, 'arrayA'), ClassdefSaveLoadHooks(35, 'arrayB')];
nestedHookArray = struct('items', hookArray, ...
  'payload', {{[ClassdefSaveLoadHooks(36, 'cellArrayA'), ...
  ClassdefSaveLoadHooks(37, 'cellArrayB')]}});
saveFile = fullfile(fixtureDirectory, 'classdef_save_load_hooks_array_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'hookArray', 'nestedHookArray');
clear hookArray nestedHookArray;
clear classes;
load(saveFile);
asserts.isequal(class(hookArray), 'ClassdefSaveLoadHooks');
asserts.isequal(length(hookArray), 2);
asserts.isequal(hookArray(1).Value, 44);
asserts.isequal(hookArray(1).Label, 'arrayA_saved_loaded');
asserts.isequal(hookArray(2).Value, 45);
asserts.isequal(hookArray(2).Label, 'arrayB_saved_loaded');
asserts.isequal(class(nestedHookArray.items), 'ClassdefSaveLoadHooks');
asserts.isequal(length(nestedHookArray.items), 2);
asserts.isequal(nestedHookArray.items(1).Value, 44);
asserts.isequal(nestedHookArray.items(2).Label, 'arrayB_saved_loaded');
asserts.isequal(class(nestedHookArray.payload{1}), 'ClassdefSaveLoadHooks');
asserts.isequal(length(nestedHookArray.payload{1}), 2);
asserts.isequal(nestedHookArray.payload{1}(1).Value, 46);
asserts.isequal(nestedHookArray.payload{1}(2).Label, 'cellArrayB_saved_loaded');
for v = matVersions
  hookArray = [ClassdefSaveLoadHooks(38, 'matArrayA'), ...
    ClassdefSaveLoadHooks(39, 'matArrayB')];
  matFile = fullfile(fixtureDirectory, ['classdef_save_load_hooks_array_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'hookArray', v{1});
  clear hookArray;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(hookArray), 'ClassdefSaveLoadHooks');
  asserts.isequal(length(hookArray), 2);
  asserts.isequal(hookArray(1).Value, 48);
  asserts.isequal(hookArray(1).Label, 'matArrayA_saved_loaded');
  asserts.isequal(hookArray(2).Value, 49);
  asserts.isequal(hookArray(2).Label, 'matArrayB_saved_loaded');
end
emptyHookArray = ClassdefSaveLoadHooks.empty(0, 2);
saveFile = fullfile(fixtureDirectory, 'classdef_save_load_hooks_empty_array_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'emptyHookArray');
clear emptyHookArray;
clear classes;
load(saveFile);
asserts.isequal(class(emptyHookArray), 'ClassdefSaveLoadHooks');
asserts.isequal(size(emptyHookArray), [0 2]);
for v = matVersions
  emptyHookArray = ClassdefSaveLoadHooks.empty(0, 3);
  matFile = fullfile(fixtureDirectory, ['classdef_save_load_hooks_empty_array_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'emptyHookArray', v{1});
  clear emptyHookArray;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(emptyHookArray), 'ClassdefSaveLoadHooks');
  asserts.isequal(size(emptyHookArray), [0 3]);
end
%=============================================================================
badSaveHook = ClassdefBadSaveHook();
badSaveFile = fullfile(fixtureDirectory, 'classdef_bad_save_hook_roundtrip.mat');
asserts.checkerror('save(badSaveFile, ''badSaveHook'');', 'saveObjectImpl must return a struct.');
%=============================================================================
protectedHookObject = ClassdefProtectedSaveLoadHooks(5);
saveFile = fullfile(fixtureDirectory, 'classdef_protected_save_load_hooks_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'protectedHookObject');
clear protectedHookObject;
clear classes;
load(saveFile);
asserts.isequal(class(protectedHookObject), 'ClassdefProtectedSaveLoadHooks');
asserts.isequal(protectedHookObject.Value, 25);
%=============================================================================
savedPoints = [ClassdefPoint(1, 2), ClassdefPoint(3, 4)];
savedPoints(2).X = 100;
savedPoints(1).Label = 'first';
saveFile = fullfile(fixtureDirectory, 'classdef_save_builtin_object_array_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedPoints');
clear savedPoints;
clear classes;
load(saveFile);
asserts.isequal(class(savedPoints), 'ClassdefPoint');
asserts.isequal(length(savedPoints), 2);
asserts.isequal(savedPoints(1).Label, 'first');
asserts.isequal(savedPoints(2).X, 100);
asserts.isequal(savedPoints(2).Y, 4);
asserts.isequal(savedPoints(1).Dimension, 2);
asserts.isequal(savedPoints(2).magnitude(), sqrt(100 * 100 + 4 * 4));
savedPointsMeta = metaclass(savedPoints);
asserts.isequal(savedPointsMeta.Name, 'ClassdefPoint');
savedExpandedPoints(3) = ClassdefPoint(11, 12);
saveFile = fullfile(fixtureDirectory, 'classdef_save_builtin_expanded_object_array_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedExpandedPoints');
clear savedExpandedPoints;
clear classes;
load(saveFile);
asserts.isequal(class(savedExpandedPoints), 'ClassdefPoint');
asserts.isequal(size(savedExpandedPoints), [1 3]);
asserts.isequal(savedExpandedPoints(1).X, 0);
asserts.isequal(savedExpandedPoints(1).Label, 'point');
asserts.isequal(savedExpandedPoints(2).Y, 0);
asserts.isequal(savedExpandedPoints(2).Label, 'point');
asserts.isequal(savedExpandedPoints(3).X, 11);
asserts.isequal(savedExpandedPoints(3).Y, 12);
%=============================================================================
nestedStruct = struct('point', ClassdefPoint(31, 32), ...
  'payload', {{ClassdefDependentValue(9), ClassdefPoint(33, 34)}});
nestedCell = {ClassdefPoint(35, 36), struct('inner', ClassdefDependentValue(10))};
saveFile = fullfile(fixtureDirectory, 'classdef_save_builtin_nested_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'nestedStruct', 'nestedCell');
clear nestedStruct nestedCell;
clear classes;
load(saveFile);
asserts.isequal(class(nestedStruct.point), 'ClassdefPoint');
asserts.isequal(nestedStruct.point.X, 31);
asserts.isequal(nestedStruct.point.Y, 32);
asserts.isequal(class(nestedStruct.payload{1}), 'ClassdefDependentValue');
asserts.isequal(nestedStruct.payload{1}.Twice, 18);
asserts.isequal(fieldnames(struct(nestedStruct.payload{1})), {'Base'});
asserts.isfalse(any(strcmp(fieldnames(struct(nestedStruct.payload{1})), 'Twice')));
asserts.isequal(class(nestedStruct.payload{2}), 'ClassdefPoint');
asserts.isequal(nestedStruct.payload{2}.Y, 34);
asserts.isequal(nestedStruct.payload{2}.Dimension, 2);
asserts.isequal(class(nestedCell{1}), 'ClassdefPoint');
asserts.isequal(nestedCell{1}.X, 35);
asserts.isequal(nestedCell{1}.Dimension, 2);
asserts.isequal(class(nestedCell{2}.inner), 'ClassdefDependentValue');
asserts.isequal(nestedCell{2}.inner.Twice, 20);
%=============================================================================
nestedStruct = struct('counter', ClassdefCounter(51), ...
  'payload', {{ClassdefPoint(52, 53), ClassdefCounter(54)}});
nestedCell = {ClassdefCounter(55), struct('inner', ClassdefCounter(56))};
saveFile = fullfile(fixtureDirectory, 'classdef_save_builtin_nested_handle_roundtrip.mat');
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
asserts.isequal(class(nestedStruct.counter), 'ClassdefCounter');
asserts.isequal(nestedStruct.counter.Count, 51);
asserts.isequal(class(nestedStruct.payload{1}), 'ClassdefPoint');
asserts.isequal(nestedStruct.payload{1}.Y, 53);
asserts.isequal(class(nestedStruct.payload{2}), 'ClassdefCounter');
asserts.isequal(nestedStruct.payload{2}.Count, 54);
asserts.isequal(class(nestedCell{1}), 'ClassdefCounter');
asserts.isequal(nestedCell{1}.Count, 55);
asserts.isequal(class(nestedCell{2}.inner), 'ClassdefCounter');
asserts.isequal(nestedCell{2}.inner.Count, 56);
delete(nestedStruct.counter);
delete(nestedStruct.payload{2});
delete(nestedCell{1});
delete(nestedCell{2}.inner);
savedExpandedCounters(3) = ClassdefCounter(71);
saveFile = fullfile(fixtureDirectory, 'classdef_save_builtin_expanded_handle_array_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedExpandedCounters');
delete(savedExpandedCounters);
clear savedExpandedCounters;
clear classes;
load(saveFile);
asserts.isequal(class(savedExpandedCounters), 'ClassdefCounter');
asserts.isequal(size(savedExpandedCounters), [1 3]);
asserts.istrue(all(isvalid(savedExpandedCounters)));
asserts.isequal(savedExpandedCounters(1).Count, 0);
asserts.isequal(savedExpandedCounters(2).Count, 0);
asserts.isequal(savedExpandedCounters(3).Count, 71);
restoredExpandedSecond = savedExpandedCounters(2);
restoredExpandedSecond.increment(4);
asserts.isequal(savedExpandedCounters(2).Count, 4);
asserts.isequal(savedExpandedCounters(1).Count, 0);
delete(savedExpandedCounters);
clear savedExpandedCounters;
emptySavedPoints = ClassdefPoint.empty(0, 3);
emptySavedCounters = ClassdefCounter.empty(0, 2);
saveFile = fullfile(fixtureDirectory, 'classdef_save_builtin_empty_object_array_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'emptySavedPoints', 'emptySavedCounters');
clear emptySavedPoints emptySavedCounters;
clear classes;
load(saveFile);
asserts.isequal(class(emptySavedPoints), 'ClassdefPoint');
asserts.isequal(size(emptySavedPoints), [0 3]);
asserts.isequal(class(emptySavedCounters), 'ClassdefCounter');
asserts.isequal(size(emptySavedCounters), [0 2]);
asserts.isequal(size(isvalid(emptySavedCounters)), [0 2]);
%=============================================================================
for v = matVersions
  savedPoints = [ClassdefPoint(5, 6), ClassdefPoint(7, 8)];
  savedPoints(2).Label = 'second';
  matFile = fullfile(fixtureDirectory, ['classdef_object_array_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'savedPoints', v{1});
  clear savedPoints;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(savedPoints), 'ClassdefPoint');
  asserts.isequal(length(savedPoints), 2);
  asserts.isequal(savedPoints(1).X, 5);
  asserts.isequal(savedPoints(2).Y, 8);
  asserts.isequal(savedPoints(2).Label, 'second');
  asserts.isequal(savedPoints(1).Dimension, 2);
  asserts.isequal(savedPoints(2).magnitude(), sqrt(7 * 7 + 8 * 8));
  savedPointsStruct = struct(savedPoints(1));
  asserts.isequal(fieldnames(savedPointsStruct), {'X'; 'Y'; 'Label'});
  savedExpandedPoints(3) = ClassdefPoint(15, 16);
  matFile = fullfile(fixtureDirectory, ['classdef_expanded_object_array_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'savedExpandedPoints', v{1});
  clear savedExpandedPoints;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(savedExpandedPoints), 'ClassdefPoint');
  asserts.isequal(size(savedExpandedPoints), [1 3]);
  asserts.isequal(savedExpandedPoints(1).X, 0);
  asserts.isequal(savedExpandedPoints(1).Label, 'point');
  asserts.isequal(savedExpandedPoints(2).Y, 0);
  asserts.isequal(savedExpandedPoints(2).Label, 'point');
  asserts.isequal(savedExpandedPoints(3).X, 15);
  asserts.isequal(savedExpandedPoints(3).Y, 16);
  emptySavedPoints = ClassdefPoint.empty(0, 4);
  matFile = fullfile(fixtureDirectory, ['classdef_empty_object_array_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'emptySavedPoints', v{1});
  clear emptySavedPoints;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(emptySavedPoints), 'ClassdefPoint');
  asserts.isequal(size(emptySavedPoints), [0 4]);
end
%=============================================================================
for v = matVersions
  nestedStruct = struct('point', ClassdefPoint(41, 42), ...
    'payload', {{ClassdefDependentValue(11), ClassdefPoint(43, 44)}});
  nestedCell = {ClassdefPoint(45, 46), struct('inner', ClassdefDependentValue(12))};
  matFile = fullfile(fixtureDirectory, ['classdef_nested_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'nestedStruct', 'nestedCell', v{1});
  clear nestedStruct nestedCell;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(nestedStruct.point), 'ClassdefPoint');
  asserts.isequal(nestedStruct.point.X, 41);
  asserts.isequal(nestedStruct.point.Y, 42);
  asserts.isequal(class(nestedStruct.payload{1}), 'ClassdefDependentValue');
  asserts.isequal(nestedStruct.payload{1}.Twice, 22);
  asserts.isequal(fieldnames(struct(nestedStruct.payload{1})), {'Base'});
  asserts.isequal(class(nestedStruct.payload{2}), 'ClassdefPoint');
  asserts.isequal(nestedStruct.payload{2}.Y, 44);
  asserts.isequal(nestedStruct.payload{2}.Dimension, 2);
  asserts.isequal(class(nestedCell{1}), 'ClassdefPoint');
  asserts.isequal(nestedCell{1}.X, 45);
  asserts.isequal(nestedCell{1}.Dimension, 2);
  asserts.isequal(class(nestedCell{2}.inner), 'ClassdefDependentValue');
  asserts.isequal(nestedCell{2}.inner.Twice, 24);
end
%=============================================================================
for v = matVersions
  nestedStruct = struct('counter', ClassdefCounter(61), ...
    'payload', {{ClassdefPoint(62, 63), ClassdefCounter(64)}});
  nestedCell = {ClassdefCounter(65), struct('inner', ClassdefCounter(66))};
  matFile = fullfile(fixtureDirectory, ['classdef_nested_handle_roundtrip', v{1}, '.mat']);
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
  asserts.isequal(class(nestedStruct.counter), 'ClassdefCounter');
  asserts.isequal(nestedStruct.counter.Count, 61);
  asserts.isequal(class(nestedStruct.payload{1}), 'ClassdefPoint');
  asserts.isequal(nestedStruct.payload{1}.Y, 63);
  asserts.isequal(class(nestedStruct.payload{2}), 'ClassdefCounter');
  asserts.isequal(nestedStruct.payload{2}.Count, 64);
  asserts.isequal(class(nestedCell{1}), 'ClassdefCounter');
  asserts.isequal(nestedCell{1}.Count, 65);
  asserts.isequal(class(nestedCell{2}.inner), 'ClassdefCounter');
  asserts.isequal(nestedCell{2}.inner.Count, 66);
  delete(nestedStruct.counter);
  delete(nestedStruct.payload{2});
  delete(nestedCell{1});
  delete(nestedCell{2}.inner);
  savedExpandedCounters(3) = ClassdefCounter(81);
  matFile = fullfile(fixtureDirectory, ['classdef_expanded_handle_array_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'savedExpandedCounters', v{1});
  delete(savedExpandedCounters);
  clear savedExpandedCounters;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(savedExpandedCounters), 'ClassdefCounter');
  asserts.isequal(size(savedExpandedCounters), [1 3]);
  asserts.istrue(all(isvalid(savedExpandedCounters)));
  asserts.isequal(savedExpandedCounters(1).Count, 0);
  asserts.isequal(savedExpandedCounters(2).Count, 0);
  asserts.isequal(savedExpandedCounters(3).Count, 81);
  restoredExpandedSecond = savedExpandedCounters(2);
  restoredExpandedSecond.increment(6);
  asserts.isequal(savedExpandedCounters(2).Count, 6);
  asserts.isequal(savedExpandedCounters(1).Count, 0);
  delete(savedExpandedCounters);
  clear savedExpandedCounters;
  emptySavedCounters = ClassdefCounter.empty(0, 5);
  matFile = fullfile(fixtureDirectory, ['classdef_empty_handle_array_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'emptySavedCounters', v{1});
  clear emptySavedCounters;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(emptySavedCounters), 'ClassdefCounter');
  asserts.isequal(size(emptySavedCounters), [0 5]);
  asserts.isequal(size(isvalid(emptySavedCounters)), [0 5]);
end
%=============================================================================
savedDependent = ClassdefDependentValue(12);
saveFile = fullfile(fixtureDirectory, 'classdef_dependent_value_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedDependent');
clear savedDependent;
clear classes;
load(saveFile);
asserts.isequal(class(savedDependent), 'ClassdefDependentValue');
asserts.isequal(savedDependent.Base, 12);
asserts.isequal(savedDependent.Twice, 24);
asserts.isequal(fieldnames(struct(savedDependent)), {'Base'});
asserts.isfalse(any(strcmp(properties(savedDependent), 'Missing')));
%=============================================================================
for v = matVersions
  savedDependent = ClassdefDependentValue(13);
  matFile = fullfile(fixtureDirectory, ['classdef_dependent_value_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'savedDependent', v{1});
  clear savedDependent;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(savedDependent), 'ClassdefDependentValue');
  asserts.isequal(savedDependent.Base, 13);
  asserts.isequal(savedDependent.Twice, 26);
end
%=============================================================================
for v = matVersions
  savedDependentHandle = ClassdefDependentHandle(14);
  matFile = fullfile(fixtureDirectory, ['classdef_dependent_handle_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'savedDependentHandle', v{1});
  delete(savedDependentHandle);
  clear savedDependentHandle;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(savedDependentHandle), 'ClassdefDependentHandle');
  asserts.isequal(savedDependentHandle.Base, 14);
  asserts.isequal(savedDependentHandle.Twice, 28);
  delete(savedDependentHandle);
end
rmpath(path_test);
%=============================================================================
