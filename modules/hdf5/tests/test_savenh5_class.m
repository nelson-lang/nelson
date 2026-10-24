%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempname(), '.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
cplx = complexObj(3, 4);
savenh5(test_h5save_file, 'cplx');
%=============================================================================
R = h5readatt(test_h5save_file, '/cplx', 'NELSON_class');
asserts.isequal(R, class(cplx));
R = h5readatt(test_h5save_file, '/cplx', 'NELSON_dimensions');
asserts.isequal(R, uint64([1; 1]));
R = h5readatt(test_h5save_file, '/cplx', 'NELSON_object');
asserts.isequal(R, uint8(1));
R = h5read(test_h5save_file, '/cplx');
asserts.isequal(class(R), 'complexObj');
asserts.isequal(R.r, 3);
asserts.isequal(R.i, 4);
R = h5read(test_h5save_file, '/cplx/0');
asserts.isequal(R, 3);
R = h5read(test_h5save_file, '/cplx/1');
asserts.isequal(R, 4);
R = h5read(test_h5save_file, '/cplx/fieldnames/0');
asserts.isequal(R, uint16('r'));
R = h5read(test_h5save_file, '/cplx/fieldnames/1');
asserts.isequal(R, uint16('i'));
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
hookObject = ClassdefSaveLoadHooks(7, 'nh5');
hookData = hookObject.saveObjectImpl();
hookRestored = ClassdefSaveLoadHooks.loadObjectImpl(hookData);
asserts.isequal(hookData.Value, 107);
asserts.isequal(hookData.Label, 'nh5_saved');
asserts.isequal(hookRestored.Value, 17);
asserts.isequal(hookRestored.Label, 'nh5_saved_loaded');
asserts.isequal(hookRestored.Twice, 34);
savenh5(test_h5save_file, '-append', 'hookObject');
R = h5readatt(test_h5save_file, '/hookObject', 'NELSON_class');
asserts.isequal(R, class(hookObject));
R = h5readatt(test_h5save_file, '/hookObject', 'NELSON_object');
asserts.isequal(R, uint8(1));
clear hookObject hookRestored hookData;
clear classes;
loadedHook = loadnh5(test_h5save_file);
asserts.isequal(class(loadedHook.hookObject), 'ClassdefSaveLoadHooks');
asserts.isequal(loadedHook.hookObject.Value, 17);
asserts.isequal(loadedHook.hookObject.Label, 'nh5_saved_loaded');
asserts.isequal(loadedHook.hookObject.Twice, 34);
asserts.isequal(fieldnames(struct(loadedHook.hookObject)), {'Value'; 'Label'});
global CLASSDEF_HANDLE_SAVE_HOOK_COUNT;
CLASSDEF_HANDLE_SAVE_HOOK_COUNT = 0;
partialHandleHookArray = [ClassdefHandleSaveLoadHooks(3), ClassdefHandleSaveLoadHooks(4)];
delete(partialHandleHookArray(2));
asserts.checkerror('savenh5(test_h5save_file, ''-append'', ''partialHandleHookArray'');', ...
  _('Invalid handle.'));
asserts.isequal(CLASSDEF_HANDLE_SAVE_HOOK_COUNT, 0);
delete(partialHandleHookArray(1));
clear partialHandleHookArray CLASSDEF_HANDLE_SAVE_HOOK_COUNT;
%=============================================================================
nestedHookStruct = struct('inner', ClassdefSaveLoadHooks(10, 'nestedNh5'), ...
  'payload', {{ClassdefSaveLoadHooks(11, 'nestedNh5Cell')}});
nestedHookCell = {ClassdefSaveLoadHooks(12, 'nh5Cell'), ...
  struct('inner', ClassdefSaveLoadHooks(13, 'nh5CellStruct'))};
savenh5(test_h5save_file, '-append', 'nestedHookStruct', 'nestedHookCell');
clear nestedHookStruct nestedHookCell;
clear classes;
loadedNestedHook = loadnh5(test_h5save_file);
asserts.isequal(class(loadedNestedHook.nestedHookStruct.inner), 'ClassdefSaveLoadHooks');
asserts.isequal(loadedNestedHook.nestedHookStruct.inner.Value, 20);
asserts.isequal(loadedNestedHook.nestedHookStruct.inner.Label, 'nestedNh5_saved_loaded');
asserts.isequal(class(loadedNestedHook.nestedHookStruct.payload{1}), 'ClassdefSaveLoadHooks');
asserts.isequal(loadedNestedHook.nestedHookStruct.payload{1}.Value, 21);
asserts.isequal(loadedNestedHook.nestedHookStruct.payload{1}.Label, 'nestedNh5Cell_saved_loaded');
asserts.isequal(class(loadedNestedHook.nestedHookCell{1}), 'ClassdefSaveLoadHooks');
asserts.isequal(loadedNestedHook.nestedHookCell{1}.Value, 22);
asserts.isequal(loadedNestedHook.nestedHookCell{1}.Label, 'nh5Cell_saved_loaded');
asserts.isequal(class(loadedNestedHook.nestedHookCell{2}.inner), 'ClassdefSaveLoadHooks');
asserts.isequal(loadedNestedHook.nestedHookCell{2}.inner.Value, 23);
asserts.isequal(loadedNestedHook.nestedHookCell{2}.inner.Label, 'nh5CellStruct_saved_loaded');
%=============================================================================
emptyNh5Points = ClassdefPoint.empty(0, 3);
emptyNh5Counters = ClassdefCounter.empty(0, 2);
savenh5(test_h5save_file, '-append', 'emptyNh5Points', 'emptyNh5Counters');
clear emptyNh5Points emptyNh5Counters;
clear classes;
loadedEmptyNh5 = loadnh5(test_h5save_file);
asserts.isequal(class(loadedEmptyNh5.emptyNh5Points), 'ClassdefPoint');
asserts.isequal(size(loadedEmptyNh5.emptyNh5Points), [0 3]);
asserts.isequal(class(loadedEmptyNh5.emptyNh5Counters), 'ClassdefCounter');
asserts.isequal(size(loadedEmptyNh5.emptyNh5Counters), [0 2]);
asserts.isequal(size(isvalid(loadedEmptyNh5.emptyNh5Counters)), [0 2]);
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
savenh5(test_h5save_file, '-append', 'T');
R = h5readatt(test_h5save_file, '/T', 'NELSON_class');
asserts.isequal(R, 'table');
R = h5readatt(test_h5save_file, '/T', 'NELSON_object');
asserts.isequal(R, uint8(1));
R = h5read(test_h5save_file, '/T');
asserts.istrue(istable(R));
asserts.isequal(class(R), 'table');
asserts.isequal(R.A, [1; 2]);
asserts.isequal(R.B, [3; 4]);
P = R.Properties;
asserts.isequal(P.RowNames, {'r1'; 'r2'});
%=============================================================================
