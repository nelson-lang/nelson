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
red = useClassdefColorRed();
blue = useClassdefColorBlue();
asserts.isequal(class(red), 'ClassdefColor');
asserts.istrue(isa(red, 'ClassdefColor'));
asserts.isequal(red.Name, 'Red');
asserts.isequal(blue.Name, 'Blue');
asserts.istrue(useClassdefColorEquality());
asserts.istrue(useClassdefColorInequality());
asserts.istrue(red.isRed());
asserts.isequal(char(red), 'Red');
persistColor = red;
persistColorFile = fullfile(fixtureDirectory, 'classdef_color_roundtrip.nh5');
save(persistColorFile, 'persistColor');
clear persistColor;
load(persistColorFile);
asserts.isequal(class(persistColor), 'ClassdefColor');
asserts.isequal(persistColor.Name, 'Red');
asserts.istrue(persistColor.isRed());
enumMembers = enumeration('ClassdefColor');
asserts.istrue(any(strcmp(enumMembers, 'Red')));
asserts.istrue(any(strcmp(enumMembers, 'Blue')));
asserts.checkerror('ClassdefEnumInheritedPropertyConflict.Red;', 'Enumeration member conflict: Red');
asserts.checkerror('ClassdefEnumInheritedMethodConflict.Blue;', 'Enumeration member conflict: Blue');
colorMeta = metaclass(red);
asserts.isequal(colorMeta.Name, 'ClassdefColor');
asserts.istrue(any(strcmp({colorMeta.EnumerationMemberList.Name}, 'Red')));
colorMetaFromQuery = ?ClassdefColor;
asserts.isequal(colorMetaFromQuery.Name, 'ClassdefColor');
colorEnumerationDetailNames = {colorMeta.EnumerationDetails.Name};
redDetail = colorMeta.EnumerationDetails(find(strcmp(colorEnumerationDetailNames, 'Red')));
asserts.isequal(redDetail.DefiningClass, 'ClassdefColor');
derivedMeta = metaclass('ClassdefDerived');
asserts.isequal(derivedMeta.Name, 'ClassdefDerived');
asserts.istrue(any(strcmp({derivedMeta.SuperclassList.Name}, 'ClassdefBase')));
asserts.istrue(any(strcmp({derivedMeta.PropertyList.Name}, 'BaseValue')));
asserts.istrue(any(strcmp({derivedMeta.MethodList.Name}, 'derivedMethod')));
asserts.istrue(isempty(derivedMeta.EventList));
derivedPropertyDetailNames = {derivedMeta.PropertyDetails.Name};
baseConstantDetail = derivedMeta.PropertyDetails(find(strcmp(derivedPropertyDetailNames, 'BaseConstant')));
asserts.isequal(baseConstantDetail.DefiningClass, 'ClassdefBase');
asserts.istrue(baseConstantDetail.Constant);
asserts.isfalse(baseConstantDetail.Abstract);
basePrivateDetail = derivedMeta.PropertyDetails(find(strcmp(derivedPropertyDetailNames, 'BasePrivateValue')));
asserts.isequal(basePrivateDetail.Access, 'private');
asserts.isequal(basePrivateDetail.GetAccess, 'private');
baseReadOnlyDetail = derivedMeta.PropertyDetails(find(strcmp(derivedPropertyDetailNames, 'BaseReadOnlyByAccess')));
asserts.isequal(baseReadOnlyDetail.GetAccess, 'public');
asserts.isequal(baseReadOnlyDetail.SetAccess, 'private');
derivedMethodDetailNames = {derivedMeta.MethodDetails.Name};
baseStaticDetail = derivedMeta.MethodDetails(find(strcmp(derivedMethodDetailNames, 'baseStatic')));
asserts.isequal(baseStaticDetail.DefiningClass, 'ClassdefBase');
asserts.istrue(baseStaticDetail.Static);
asserts.isfalse(baseStaticDetail.Abstract);
hiddenMethodDetail = derivedMeta.MethodDetails(find(strcmp(derivedMethodDetailNames, 'hiddenMethod')));
asserts.istrue(hiddenMethodDetail.Hidden);
privateMethodDetail = derivedMeta.MethodDetails(find(strcmp(derivedMethodDetailNames, 'privateMethod')));
asserts.isequal(privateMethodDetail.Access, 'private');
sealedMethodDetail = derivedMeta.MethodDetails(find(strcmp(derivedMethodDetailNames, 'sealedMethod')));
asserts.istrue(sealedMethodDetail.Sealed);
%=============================================================================
rmpath(path_test);
rmdir(fixtureDirectory, 's');
%=============================================================================
