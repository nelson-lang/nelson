%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
%=============================================================================
matVersions = {'-v7', '-v7.3'};
savedDynamic = ClassdefDynamicProps();
savedToken = addprop(savedDynamic, 'Extra');
savedDynamic.Extra = 71;
numericToken = addprop(savedDynamic, 'NumericExtra');
numericToken.ValidationExpression = '(1, 1) int32';
savedDynamic.NumericExtra = 7;
asserts.isequal(class(savedDynamic.NumericExtra), 'int32');
asserts.isequal(savedDynamic.NumericExtra, int32(7));
setMethodToken = addprop(savedDynamic, 'SetterExtra');
setMethodToken.ValidationExpression = '(1, 1) single';
setMethodToken.SetMethod = @(obj, value) obj.writeDynamicExtra(value);
savedDynamic.SetterExtra = 8;
asserts.isequal(class(savedDynamic.StoredExtra), 'single');
asserts.isequal(savedDynamic.StoredExtra, single(16));
rowToken = addprop(savedDynamic, 'RowExtra');
rowToken.ValidationExpression = '(1, 2) single';
savedDynamic.RowExtra = 9;
asserts.isequal(class(savedDynamic.RowExtra), 'single');
asserts.isequal(savedDynamic.RowExtra, single([9 9]));
rowSetMethodToken = addprop(savedDynamic, 'SetterRowExtra');
rowSetMethodToken.ValidationExpression = '(1, 2) single';
rowSetMethodToken.SetMethod = @(obj, value) obj.writeDynamicExtra(value);
savedDynamic.SetterRowExtra = 10;
asserts.isequal(class(savedDynamic.StoredExtra), 'single');
asserts.isequal(savedDynamic.StoredExtra, single([20 20]));
scratchToken = addprop(savedDynamic, 'Scratch');
savedDynamic.Scratch = 81;
scratchToken.Transient = true;
saveFile = fullfile(fixtureDirectory, 'classdef_dynamicprops_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedDynamic');
delete(savedDynamic);
clear savedDynamic savedToken scratchToken;
clear classes;
load(saveFile);
asserts.isequal(class(savedDynamic), 'ClassdefDynamicProps');
asserts.istrue(isprop(savedDynamic, 'Extra'));
asserts.isequal(savedDynamic.Extra, 71);
asserts.isfalse(isprop(savedDynamic, 'Scratch'));
savedDynamicMeta = metaclass(savedDynamic);
savedDynamicNames = {savedDynamicMeta.PropertyDetails.Name};
savedDynamicExtra = savedDynamicMeta.PropertyDetails(find(strcmp(savedDynamicNames, 'Extra')));
asserts.istrue(savedDynamicExtra.Dynamic);
asserts.isfalse(savedDynamicExtra.Abstract);
asserts.isfalse(savedDynamicExtra.Transient);
savedDynamicStruct = struct(savedDynamic);
asserts.isequal(savedDynamicStruct.Extra, 71);
delete(savedDynamic);
clear savedDynamic savedDynamicMeta savedDynamicNames savedDynamicExtra savedDynamicStruct;
%=============================================================================
for v = matVersions
  savedDynamic = ClassdefDynamicProps();
  savedToken = addprop(savedDynamic, 'Extra');
  savedDynamic.Extra = 72;
  scratchToken = addprop(savedDynamic, 'Scratch');
  savedDynamic.Scratch = 82;
  scratchToken.Transient = true;
  matFile = fullfile(fixtureDirectory, ['classdef_dynamicprops_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'savedDynamic', v{1});
  delete(savedDynamic);
  clear savedDynamic savedToken scratchToken;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(savedDynamic), 'ClassdefDynamicProps');
  asserts.istrue(isprop(savedDynamic, 'Extra'));
  asserts.isequal(savedDynamic.Extra, 72);
  asserts.isfalse(isprop(savedDynamic, 'Scratch'));
  savedDynamicStruct = struct(savedDynamic);
  asserts.isequal(savedDynamicStruct.Extra, 72);
  delete(savedDynamic);
  clear savedDynamic savedDynamicStruct;
end
%=============================================================================
firstDynamic = ClassdefDynamicProps();
addprop(firstDynamic, 'Extra');
firstDynamic.Extra = 73;
secondDynamic = ClassdefDynamicProps();
addprop(secondDynamic, 'Extra');
secondDynamic.Extra = 74;
scratchToken = addprop(secondDynamic, 'Scratch');
secondDynamic.Scratch = 84;
scratchToken.Transient = true;
savedDynamicArray = [firstDynamic, secondDynamic];
saveFile = fullfile(fixtureDirectory, 'classdef_dynamicprops_array_roundtrip.mat');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'savedDynamicArray');
delete(savedDynamicArray);
clear firstDynamic secondDynamic scratchToken savedDynamicArray;
clear classes;
load(saveFile);
asserts.isequal(class(savedDynamicArray), 'ClassdefDynamicProps');
asserts.isequal(size(savedDynamicArray), [1 2]);
asserts.istrue(isprop(savedDynamicArray(1), 'Extra'));
asserts.istrue(isprop(savedDynamicArray(2), 'Extra'));
asserts.isequal(savedDynamicArray(1).Extra, 73);
asserts.isequal(savedDynamicArray(2).Extra, 74);
asserts.isfalse(isprop(savedDynamicArray(1), 'Scratch'));
asserts.isfalse(isprop(savedDynamicArray(2), 'Scratch'));
delete(savedDynamicArray);
clear savedDynamicArray;
%=============================================================================
for v = matVersions
  firstDynamic = ClassdefDynamicProps();
  addprop(firstDynamic, 'Extra');
  firstDynamic.Extra = 75;
  secondDynamic = ClassdefDynamicProps();
  addprop(secondDynamic, 'Extra');
  secondDynamic.Extra = 76;
  scratchToken = addprop(secondDynamic, 'Scratch');
  secondDynamic.Scratch = 86;
  scratchToken.Transient = true;
  savedDynamicArray = [firstDynamic, secondDynamic];
  matFile = fullfile(fixtureDirectory, ['classdef_dynamicprops_array_roundtrip', v{1}, '.mat']);
  if isfile(matFile)
    rmfile(matFile);
  end
  savemat(matFile, 'savedDynamicArray', v{1});
  delete(savedDynamicArray);
  clear firstDynamic secondDynamic scratchToken savedDynamicArray;
  clear classes;
  loadmat(matFile);
  asserts.isequal(class(savedDynamicArray), 'ClassdefDynamicProps');
  asserts.isequal(size(savedDynamicArray), [1 2]);
  asserts.istrue(isprop(savedDynamicArray(1), 'Extra'));
  asserts.istrue(isprop(savedDynamicArray(2), 'Extra'));
  asserts.isequal(savedDynamicArray(1).Extra, 75);
  asserts.isequal(savedDynamicArray(2).Extra, 76);
  asserts.isfalse(isprop(savedDynamicArray(1), 'Scratch'));
  asserts.isfalse(isprop(savedDynamicArray(2), 'Scratch'));
  delete(savedDynamicArray);
  clear savedDynamicArray;
end
%=============================================================================
rmpath(path_test);
rmdir(fixtureDirectory, 's');
%=============================================================================
