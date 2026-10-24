%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
matver = {'-v7', '-v7.3'};
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
cplx = complexObj(3, 4);
REF = struct();
REF.r = 3;
REF.i = 4;
%=============================================================================
for v = matver
  test_file_mat = [tempname(), '.mat'];
  savemat(test_file_mat, 'cplx', v{1});
  clear cplx;
  loadmat(test_file_mat);
  asserts.isequal(cplx.r, REF.r);
  asserts.isequal(cplx.i, REF.i);
  rmfile(test_file_mat);
end
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
for v = matver
  hookObject = ClassdefSaveLoadHooks(9, 'mat');
  hookData = hookObject.saveObjectImpl();
  hookRestored = ClassdefSaveLoadHooks.loadObjectImpl(hookData);
  asserts.isequal(hookData.Value, 109);
  asserts.isequal(hookData.Label, 'mat_saved');
  asserts.isequal(hookRestored.Value, 19);
  asserts.isequal(hookRestored.Label, 'mat_saved_loaded');
  asserts.isequal(hookRestored.Twice, 38);
  test_file_mat = [tempname(), '.mat'];
  savemat(test_file_mat, 'hookObject', v{1});
  clear hookObject hookRestored hookData;
  clear classes;
  loadmat(test_file_mat);
  asserts.isequal(class(hookObject), 'ClassdefSaveLoadHooks');
  asserts.isequal(hookObject.Value, 19);
  asserts.isequal(hookObject.Label, 'mat_saved_loaded');
  asserts.isequal(hookObject.Twice, 38);
  asserts.isequal(fieldnames(struct(hookObject)), {'Value'; 'Label'});
  global CLASSDEF_HANDLE_SAVE_HOOK_COUNT;
  CLASSDEF_HANDLE_SAVE_HOOK_COUNT = 0;
  partialHandleHookArray = [ClassdefHandleSaveLoadHooks(5), ClassdefHandleSaveLoadHooks(6)];
  delete(partialHandleHookArray(2));
  asserts.checkerror('savemat(test_file_mat, ''partialHandleHookArray'', v{1});', ...
    _('Invalid handle.'));
  asserts.isequal(CLASSDEF_HANDLE_SAVE_HOOK_COUNT, 0);
  delete(partialHandleHookArray(1));
  clear partialHandleHookArray CLASSDEF_HANDLE_SAVE_HOOK_COUNT;
end
%=============================================================================
for v = matver
  nestedHookStruct = struct('inner', ClassdefSaveLoadHooks(14, 'nestedMat'), ...
    'payload', {{ClassdefSaveLoadHooks(15, 'nestedMatCell')}});
  nestedHookCell = {ClassdefSaveLoadHooks(16, 'matCell'), ...
    struct('inner', ClassdefSaveLoadHooks(17, 'matCellStruct'))};
  test_file_mat = [tempname(), '.mat'];
  savemat(test_file_mat, 'nestedHookStruct', 'nestedHookCell', v{1});
  clear nestedHookStruct nestedHookCell;
  clear classes;
  loadmat(test_file_mat);
  asserts.isequal(class(nestedHookStruct.inner), 'ClassdefSaveLoadHooks');
  asserts.isequal(nestedHookStruct.inner.Value, 24);
  asserts.isequal(nestedHookStruct.inner.Label, 'nestedMat_saved_loaded');
  asserts.isequal(class(nestedHookStruct.payload{1}), 'ClassdefSaveLoadHooks');
  asserts.isequal(nestedHookStruct.payload{1}.Value, 25);
  asserts.isequal(nestedHookStruct.payload{1}.Label, 'nestedMatCell_saved_loaded');
  asserts.isequal(class(nestedHookCell{1}), 'ClassdefSaveLoadHooks');
  asserts.isequal(nestedHookCell{1}.Value, 26);
  asserts.isequal(nestedHookCell{1}.Label, 'matCell_saved_loaded');
  asserts.isequal(class(nestedHookCell{2}.inner), 'ClassdefSaveLoadHooks');
  asserts.isequal(nestedHookCell{2}.inner.Value, 27);
  asserts.isequal(nestedHookCell{2}.inner.Label, 'matCellStruct_saved_loaded');
  rmfile(test_file_mat);
end
%=============================================================================
for v = matver
  emptyMatPoints = ClassdefPoint.empty(0, 3);
  emptyMatCounters = ClassdefCounter.empty(0, 2);
  test_file_mat = [tempname(), '.mat'];
  savemat(test_file_mat, 'emptyMatPoints', 'emptyMatCounters', v{1});
  clear emptyMatPoints emptyMatCounters;
  clear classes;
  loadmat(test_file_mat);
  asserts.isequal(class(emptyMatPoints), 'ClassdefPoint');
  asserts.isequal(size(emptyMatPoints), [0 3]);
  asserts.isequal(class(emptyMatCounters), 'ClassdefCounter');
  asserts.isequal(size(emptyMatCounters), [0 2]);
  asserts.isequal(size(isvalid(emptyMatCounters)), [0 2]);
  rmfile(test_file_mat);
end
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
for v = matver
  test_file_mat = [tempname(), '.mat'];
  savemat(test_file_mat, 'T', v{1});
  clear T;
  loadmat(test_file_mat);
  asserts.istrue(istable(T));
  asserts.isequal(class(T), 'table');
  asserts.isequal(T.A, [1; 2]);
  asserts.isequal(T.B, [3; 4]);
  P = T.Properties;
  asserts.isequal(P.RowNames, {'r1'; 'r2'});
  rmfile(test_file_mat);
end
%=============================================================================
