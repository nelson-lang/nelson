%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
%=============================================================================
nestedPoint = ClassdefPoint(7, 8);
nestedColor = useClassdefColorBlue();
nestedStruct = struct('point', nestedPoint, 'color', nestedColor);
nestedCell = {nestedPoint, nestedColor};
saveFile = fullfile(fixtureDirectory, 'classdef_nested_roundtrip.nh5');
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'nestedStruct', 'nestedCell');
clear nestedStruct nestedCell nestedPoint nestedColor;
clear classes;
load(saveFile);
asserts.isequal(class(nestedStruct.point), 'ClassdefPoint');
asserts.isequal(nestedStruct.point.X, 7);
asserts.isequal(nestedStruct.point.Y, 8);
asserts.isequal(class(nestedStruct.color), 'ClassdefColor');
asserts.isequal(nestedStruct.color.Name, 'Blue');
asserts.isequal(class(nestedCell{1}), 'ClassdefPoint');
asserts.isequal(nestedCell{1}.Y, 8);
asserts.isequal(class(nestedCell{2}), 'ClassdefColor');
asserts.isequal(nestedCell{2}.Name, 'Blue');
%=============================================================================
matFile = fullfile(fixtureDirectory, 'classdef_nested_roundtrip.mat');
if isfile(matFile)
  rmfile(matFile);
end
nestedStruct = struct('point', ClassdefPoint(11, 12), 'color', useClassdefColorRed());
nestedCell = {ClassdefPoint(13, 14), useClassdefColorBlue()};
savemat(matFile, 'nestedStruct', 'nestedCell', '-v7');
clear nestedStruct nestedCell;
clear classes;
loadmat(matFile);
asserts.isequal(class(nestedStruct.point), 'ClassdefPoint');
asserts.isequal(nestedStruct.point.X, 11);
asserts.isequal(class(nestedStruct.color), 'ClassdefColor');
asserts.isequal(nestedStruct.color.Name, 'Red');
asserts.isequal(class(nestedCell{1}), 'ClassdefPoint');
asserts.isequal(nestedCell{1}.X, 13);
asserts.isequal(class(nestedCell{2}), 'ClassdefColor');
asserts.isequal(nestedCell{2}.Name, 'Blue');
%=============================================================================
enumMembers = enumeration('ClassdefEnumWithConstructor');
asserts.istrue(any(strcmp(enumMembers, 'Low')));
asserts.istrue(any(strcmp(enumMembers, 'High')));
enumConstructorMeta = ?ClassdefEnumWithConstructor;
enumConstructorDetailNames = {enumConstructorMeta.EnumerationDetails.Name};
lowDetail = enumConstructorMeta.EnumerationDetails(find(strcmp(enumConstructorDetailNames, 'Low')));
highDetail = enumConstructorMeta.EnumerationDetails(find(strcmp(enumConstructorDetailNames, 'High')));
asserts.isequal(lowDetail.ConstructorArguments, '1');
asserts.isequal(highDetail.ConstructorArguments, '2');
enumLow = useClassdefEnumWithConstructorLow();
enumHigh = useClassdefEnumWithConstructorHigh();
asserts.isequal(class(enumLow), 'ClassdefEnumWithConstructor');
asserts.istrue(isa(enumLow, 'ClassdefEnumWithConstructor'));
asserts.isequal(enumLow.Name, 'Low');
asserts.isequal(enumHigh.Name, 'High');
asserts.isequal(enumLow.Code, 1);
asserts.isequal(enumHigh.Code, 2);
asserts.isequal(char(enumLow), 'Low');
asserts.istrue(useClassdefEnumWithConstructorEquality());
asserts.istrue(useClassdefEnumWithConstructorInequality());
asserts.isfalse(enumLow == enumHigh);
asserts.isfalse(enumLow.isHigh());
asserts.istrue(enumHigh.isHigh());
typedEnumLow = ClassdefEnumUint32.Low;
typedEnumHigh = ClassdefEnumUint32.High;
asserts.isequal(class(typedEnumLow), 'ClassdefEnumUint32');
asserts.istrue(isa(typedEnumLow, 'ClassdefEnumUint32'));
asserts.istrue(isa(typedEnumLow, 'uint32'));
asserts.isequal(uint32(typedEnumLow), uint32(1));
asserts.isequal(uint32(typedEnumHigh), uint32(2));
asserts.isequal(char(typedEnumHigh), 'High');
asserts.istrue(typedEnumHigh.isHigh());
asserts.isfalse(typedEnumLow == typedEnumHigh);
typedEnumFromValue = ClassdefEnumUint32(1);
typedEnumArrayFromValue = ClassdefEnumUint32([1 2]);
asserts.isequal(char(typedEnumFromValue), 'Low');
asserts.isequal(char(typedEnumArrayFromValue(2)), 'High');
%=============================================================================
% enumeration deriving from logical (base machinery must apply the same way
% it does for the numeric bases above)
logicalOff = ClassdefEnumLogical.Off;
logicalOn = ClassdefEnumLogical.On;
asserts.isequal(class(logicalOn), 'ClassdefEnumLogical');
asserts.istrue(isa(logicalOn, 'ClassdefEnumLogical'));
asserts.isequal(logical(logicalOff), false);
asserts.isequal(logical(logicalOn), true);
asserts.isequal(char(logicalOn), 'On');
asserts.istrue(logicalOn.isOn());
asserts.isfalse(logicalOff.isOn());
asserts.istrue(logicalOn == logicalOn);
asserts.isfalse(logicalOn == logicalOff);
asserts.istrue(logicalOn ~= logicalOff);
% comparison against a plain logical uses the underlying value
asserts.istrue(logicalOn == true);
asserts.isfalse(logicalOn == false);
% comparison against text matches the member name
asserts.istrue(logicalOn == 'On');
asserts.isfalse(logicalOn == 'Off');
logicalFromText = ClassdefEnumLogical('Off');
logicalFromValue = ClassdefEnumLogical(true);
logicalArrayFromValue = ClassdefEnumLogical([false true 2]);
asserts.isequal(char(logicalFromText), 'Off');
asserts.isequal(char(logicalFromValue), 'On');
asserts.isequal(char(logicalArrayFromValue(1)), 'Off');
asserts.isequal(char(logicalArrayFromValue(2)), 'On');
asserts.isequal(char(logicalArrayFromValue(3)), 'On');
logicalDisplayText = evalc('ClassdefEnumLogical(true)');
asserts.istrue(contains(logicalDisplayText, 'ClassdefEnumLogical'));
asserts.istrue(contains(logicalDisplayText, 'enumeration'));
asserts.istrue(contains(logicalDisplayText, 'On'));
asserts.isfalse(contains(logicalDisplayText, 'with properties'));
asserts.isfalse(contains(logicalDisplayText, 'Ordinal'));
%=============================================================================
rmpath(path_test);
rmdir(fixtureDirectory, 's');
%=============================================================================
