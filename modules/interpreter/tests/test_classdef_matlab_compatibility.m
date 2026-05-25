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
assert_isequal(parsefile([path_test, '/ClassdefAttributeSyntax.m']), 'classdef');
assert_isequal(parsefile([path_test, '/ClassdefEnumWithConstructor.m']), 'classdef');
%=============================================================================
directOrigin = ClassdefPoint.origin();
assert_isequal(class(directOrigin), 'ClassdefPoint');
assert_isequal(directOrigin.X, 0);
assert_isequal(directOrigin.Y, 0);
assert_isequal(ClassdefPoint.Dimension, 2);
directColor = ClassdefColor.Red;
assert_isequal(class(directColor), 'ClassdefColor');
assert_isequal(directColor.Name, 'Red');
directEnumHigh = ClassdefEnumWithConstructor.High;
assert_isequal(class(directEnumHigh), 'ClassdefEnumWithConstructor');
assert_isequal(directEnumHigh.Name, 'High');
assert_isequal(directEnumHigh.Code, 2);
directPackaged = classdefpkg.ClassdefPackaged.origin();
assert_isequal(class(directPackaged), 'classdefpkg.ClassdefPackaged');
assert_isequal(directPackaged.Value, 0);
assert_isequal(classdefpkg.ClassdefPackaged.ConstantValue, 12);
%=============================================================================
target = ClassdefAccessTarget(19);
friend = ClassdefAccessFriend();
friendChild = ClassdefAccessFriendChild();
assert_isequal(target.readFromDefiningClass(), 19);
assert_isequal(friend.readFriendMethod(target), 19);
assert_isequal(friend.readFriendDirectMethod(target), 20);
assert_isequal(friend.readFriendProperty(target), 19);
assert_isequal(friendChild.readAsChild(target), 19);
assert_isequal(friendChild.readDirectAsChild(target), 20);
assert_isequal(friendChild.readPropertyAsChild(target), 19);
target = friend.setFriendProperty(target, 23);
assert_isequal(friend.readFriendProperty(target), 23);
assert_isequal(friend.readFriendDirectMethod(target), 24);
target = friendChild.setPropertyAsChild(target, 31);
assert_isequal(friendChild.readPropertyAsChild(target), 31);
assert_isequal(friend.readFriendProperty(target), 31);
targetMethods = methods('ClassdefAccessTarget');
targetProperties = properties('ClassdefAccessTarget');
assert_isfalse(any(strcmp(targetMethods, 'friendOnly')));
assert_isfalse(any(strcmp(targetMethods, 'friendOnlyDirect')));
assert_isfalse(any(strcmp(targetProperties, 'FriendValue')));
assert_isfalse(ismethod(target, 'friendOnly'));
assert_isfalse(ismethod(target, 'friendOnlyDirect'));
assert_istrue(isprop(target, 'FriendValue'));
targetMeta = metaclass('ClassdefAccessTarget');
targetPropertyDetailNames = {targetMeta.PropertyDetails.Name};
friendValueDetail = targetMeta.PropertyDetails(find(strcmp(targetPropertyDetailNames, 'FriendValue')));
assert_isequal(friendValueDetail.Access, '?ClassdefAccessFriend');
assert_istrue(any(strcmp(friendValueDetail.Attributes, 'Access = ?ClassdefAccessFriend')));
targetMethodDetailNames = {targetMeta.MethodDetails.Name};
friendOnlyDetail = targetMeta.MethodDetails(find(strcmp(targetMethodDetailNames, 'friendOnly')));
assert_isequal(friendOnlyDetail.Access, '{?ClassdefAccessFriend}');
assert_istrue(any(strcmp(friendOnlyDetail.Attributes, 'Access = {?ClassdefAccessFriend}')));
friendOnlyDirectDetail = targetMeta.MethodDetails(find(strcmp(targetMethodDetailNames, 'friendOnlyDirect')));
assert_isequal(friendOnlyDirectDetail.Access, '?ClassdefAccessFriend');
assert_istrue(any(strcmp(friendOnlyDirectDetail.Attributes, 'Access = ?ClassdefAccessFriend')));
assert_checkerror('target.FriendValue;', 'Cannot get property: FriendValue');
assert_checkerror('target.FriendValue = 40;', 'Cannot set property: FriendValue');
try
  target.friendOnly();
  error('Access-list method call should fail outside an allowed class.');
catch M
  assert_istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, 'Undefined'));
end
try
  target.friendOnlyDirect();
  error('Direct metaclass access method call should fail outside an allowed class.');
catch M
  assert_istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, 'Undefined'));
end
%=============================================================================
points = [ClassdefPoint(1, 2), ClassdefPoint(3, 4)];
assert_isequal(class(points), 'ClassdefPoint');
assert_isequal(length(points), 2);
assert_isequal(points(1).X, 1);
assert_isequal(points(2).Y, 4);
points(2).X = 99;
points(1).Label = 'first';
assert_isequal(points(1).Label, 'first');
assert_isequal(points(2).X, 99);
points(2) = points(2).shift(1, 2);
assert_isequal(points(2).X, 100);
assert_isequal(points(2).Y, 6);
points(2) = ClassdefPoint(13, 24);
assert_isequal(points(2).X, 13);
assert_isequal(points(2).Y, 24);
points([1, 2]) = [ClassdefPoint(5, 6), ClassdefPoint(7, 8)];
assert_isequal(points(1).X, 5);
assert_isequal(points(2).Y, 8);
try
  points(1) = ClassdefDerived(9);
  error('Wrong class assignment should fail.');
catch M
  assert_istrue(contains(M.message, 'Cannot promote to another class type array') ...
    || contains(M.message, 'Invalid indexing'));
end
%=============================================================================
dependentValue = ClassdefDependentValue(6);
assert_isequal(dependentValue.Base, 6);
assert_isequal(dependentValue.Twice, 12);
dependentValue.Twice = 40;
assert_isequal(dependentValue.Base, 20);
assert_isequal(dependentValue.Twice, 40);
assert_isequal(dependentValue.ReadOnlyTwice, 40);
dependentValue = dependentValue.setReadOnlyTwiceInside(50);
assert_isequal(dependentValue.Base, 25);
assert_checkerror('dependentValue.ReadOnlyTwice = 60;', 'Cannot set property: ReadOnlyTwice');
dependentProperties = properties(dependentValue);
assert_istrue(any(strcmp(dependentProperties, 'Twice')));
assert_istrue(any(strcmp(dependentProperties, 'ReadOnlyTwice')));
dependentMethods = methods(dependentValue);
assert_isfalse(any(strcmp(dependentMethods, 'get.Twice')));
assert_isfalse(any(strcmp(dependentMethods, 'set.Twice')));
dependentMeta = metaclass(dependentValue);
assert_istrue(any(strcmp(dependentMeta.PropertyList, 'Twice')));
dependentPropertyDetailNames = {dependentMeta.PropertyDetails.Name};
twiceDetail = dependentMeta.PropertyDetails(find(strcmp(dependentPropertyDetailNames, 'Twice')));
assert_istrue(twiceDetail.Dependent);
readOnlyTwiceDetail = dependentMeta.PropertyDetails(find(strcmp(dependentPropertyDetailNames, 'ReadOnlyTwice')));
assert_istrue(readOnlyTwiceDetail.Dependent);
assert_isequal(readOnlyTwiceDetail.SetAccess, 'private');
dependentArray = [ClassdefDependentValue(2), ClassdefDependentValue(4)];
dependentTwice = [dependentArray.Twice];
assert_isequal(dependentTwice, [4, 8]);
dependentHandle = ClassdefDependentHandle(7);
assert_isequal(dependentHandle.Twice, 14);
dependentHandle.Twice = 30;
assert_isequal(dependentHandle.Base, 15);
delete(dependentHandle);
%=============================================================================
reload_dir = [tempdir(), 'nelson_classdef_reload'];
if ~isdir(reload_dir)
  mkdir(reload_dir);
end
reload_file = [reload_dir, '/ClassdefReloadable.m'];
reloadSource = ["classdef ClassdefReloadable"; ...
  "  properties"; ...
  "    Payload = 1"; ...
  "  end"; ...
  "  properties (Constant)"; ...
  "    Version = 1"; ...
  "    ObsoleteConstant = 101"; ...
  "  end"; ...
  "  methods"; ...
  "    function r = value(obj)"; ...
  "      r = 1;"; ...
  "    end"; ...
  "  end"; ...
  "  methods (Static)"; ...
  "    function r = staticValue()"; ...
  "      r = 10;"; ...
  "    end"; ...
  "    function r = oldStatic()"; ...
  "      r = 100;"; ...
  "    end"; ...
  "  end"; ...
  "end"];
filewrite(reload_file, reloadSource);
addpath(reload_dir);
reloadObj = ClassdefReloadable();
assert_isequal(reloadObj.value(), 1);
assert_isequal(reloadObj.Payload, 1);
assert_isequal(ClassdefReloadable.Version, 1);
assert_isequal(ClassdefReloadable.ObsoleteConstant, 101);
assert_isequal(ClassdefReloadable.staticValue(), 10);
assert_isequal(ClassdefReloadable.oldStatic(), 100);
reloadMeta = metaclass('ClassdefReloadable');
assert_istrue(any(strcmp(reloadMeta.PropertyList, 'Payload')));
assert_istrue(any(strcmp(reloadMeta.PropertyList, 'Version')));
assert_istrue(any(strcmp(reloadMeta.PropertyList, 'ObsoleteConstant')));
assert_istrue(any(strcmp(reloadMeta.MethodList, 'staticValue')));
assert_istrue(any(strcmp(reloadMeta.MethodList, 'oldStatic')));
reloadSource = ["classdef ClassdefReloadable"; ...
  "  properties"; ...
  "    Payload = 2"; ...
  "    ExtraInstance = 3"; ...
  "  end"; ...
  "  properties (Constant)"; ...
  "    Version = 2"; ...
  "    Extra = 202"; ...
  "  end"; ...
  "  methods"; ...
  "    function r = value(obj)"; ...
  "      r = 2;"; ...
  "    end"; ...
  "  end"; ...
  "  methods (Static)"; ...
  "    function r = staticValue()"; ...
  "      r = 20;"; ...
  "    end"; ...
  "    function r = newStatic()"; ...
  "      r = 200;"; ...
  "    end"; ...
  "  end"; ...
  "end"];
filewrite(reload_file, reloadSource);
clear classes;
reloadObj = ClassdefReloadable();
assert_isequal(reloadObj.value(), 2);
assert_isequal(reloadObj.Payload, 2);
assert_isequal(reloadObj.ExtraInstance, 3);
assert_isequal(ClassdefReloadable.Version, 2);
assert_isequal(ClassdefReloadable.Extra, 202);
assert_isequal(ClassdefReloadable.staticValue(), 20);
assert_isequal(ClassdefReloadable.newStatic(), 200);
reloadMeta = metaclass('ClassdefReloadable');
assert_istrue(any(strcmp(reloadMeta.PropertyList, 'Payload')));
assert_istrue(any(strcmp(reloadMeta.PropertyList, 'ExtraInstance')));
assert_istrue(any(strcmp(reloadMeta.PropertyList, 'Version')));
assert_istrue(any(strcmp(reloadMeta.PropertyList, 'Extra')));
assert_isfalse(any(strcmp(reloadMeta.PropertyList, 'ObsoleteConstant')));
assert_istrue(any(strcmp(reloadMeta.MethodList, 'staticValue')));
assert_istrue(any(strcmp(reloadMeta.MethodList, 'newStatic')));
assert_isfalse(any(strcmp(reloadMeta.MethodList, 'oldStatic')));
try
  ClassdefReloadable.ObsoleteConstant;
  error('Obsolete constant should be unavailable after clear classes.');
catch M
  assert_istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Undefined classdef member'));
end
try
  ClassdefReloadable.oldStatic();
  error('Obsolete static method should be unavailable after clear classes.');
catch M
  assert_istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Undefined classdef member') ...
    || contains(M.message, 'Undefined'));
end
reloadNested = struct('object', reloadObj, 'payload', {{ClassdefReloadable()}});
reloadSaveFile = [tempdir(), 'classdef_reload_nested_roundtrip.nh5'];
if isfile(reloadSaveFile)
  rmfile(reloadSaveFile);
end
save(reloadSaveFile, 'reloadNested');
clear reloadNested reloadObj;
clear classes;
load(reloadSaveFile);
assert_isequal(class(reloadNested.object), 'ClassdefReloadable');
assert_isequal(reloadNested.object.Payload, 2);
assert_isequal(reloadNested.object.ExtraInstance, 3);
assert_isequal(class(reloadNested.payload{1}), 'ClassdefReloadable');
assert_isequal(reloadNested.payload{1}.Payload, 2);
assert_isequal(reloadNested.payload{1}.value(), 2);
rmpath(reload_dir);
rmdir(reload_dir, 's');
%=============================================================================
nestedPoint = ClassdefPoint(7, 8);
nestedColor = useClassdefColorBlue();
nestedStruct = struct('point', nestedPoint, 'color', nestedColor);
nestedCell = {nestedPoint, nestedColor};
saveFile = [tempdir(), 'classdef_nested_roundtrip.nh5'];
if isfile(saveFile)
  rmfile(saveFile);
end
save(saveFile, 'nestedStruct', 'nestedCell');
clear nestedStruct nestedCell nestedPoint nestedColor;
clear classes;
load(saveFile);
assert_isequal(class(nestedStruct.point), 'ClassdefPoint');
assert_isequal(nestedStruct.point.X, 7);
assert_isequal(nestedStruct.point.Y, 8);
assert_isequal(class(nestedStruct.color), 'ClassdefColor');
assert_isequal(nestedStruct.color.Name, 'Blue');
assert_isequal(class(nestedCell{1}), 'ClassdefPoint');
assert_isequal(nestedCell{1}.Y, 8);
assert_isequal(class(nestedCell{2}), 'ClassdefColor');
assert_isequal(nestedCell{2}.Name, 'Blue');
%=============================================================================
matFile = [tempdir(), 'classdef_nested_roundtrip.mat'];
if isfile(matFile)
  rmfile(matFile);
end
nestedStruct = struct('point', ClassdefPoint(11, 12), 'color', useClassdefColorRed());
nestedCell = {ClassdefPoint(13, 14), useClassdefColorBlue()};
savemat(matFile, 'nestedStruct', 'nestedCell', '-v7');
clear nestedStruct nestedCell;
clear classes;
loadmat(matFile);
assert_isequal(class(nestedStruct.point), 'ClassdefPoint');
assert_isequal(nestedStruct.point.X, 11);
assert_isequal(class(nestedStruct.color), 'ClassdefColor');
assert_isequal(nestedStruct.color.Name, 'Red');
assert_isequal(class(nestedCell{1}), 'ClassdefPoint');
assert_isequal(nestedCell{1}.X, 13);
assert_isequal(class(nestedCell{2}), 'ClassdefColor');
assert_isequal(nestedCell{2}.Name, 'Blue');
%=============================================================================
enumMembers = enumeration('ClassdefEnumWithConstructor');
assert_istrue(any(strcmp(enumMembers, 'Low')));
assert_istrue(any(strcmp(enumMembers, 'High')));
enumConstructorMeta = ?ClassdefEnumWithConstructor;
enumConstructorDetailNames = {enumConstructorMeta.EnumerationDetails.Name};
lowDetail = enumConstructorMeta.EnumerationDetails(find(strcmp(enumConstructorDetailNames, 'Low')));
highDetail = enumConstructorMeta.EnumerationDetails(find(strcmp(enumConstructorDetailNames, 'High')));
assert_isequal(lowDetail.ConstructorArguments, '1');
assert_isequal(highDetail.ConstructorArguments, '2');
enumLow = useClassdefEnumWithConstructorLow();
enumHigh = useClassdefEnumWithConstructorHigh();
assert_isequal(class(enumLow), 'ClassdefEnumWithConstructor');
assert_istrue(isa(enumLow, 'ClassdefEnumWithConstructor'));
assert_isequal(enumLow.Name, 'Low');
assert_isequal(enumHigh.Name, 'High');
assert_isequal(enumLow.Code, 1);
assert_isequal(enumHigh.Code, 2);
assert_isequal(char(enumLow), 'Low');
assert_istrue(useClassdefEnumWithConstructorEquality());
assert_istrue(useClassdefEnumWithConstructorInequality());
assert_isfalse(enumLow == enumHigh);
assert_isfalse(enumLow.isHigh());
assert_istrue(enumHigh.isHigh());
%=============================================================================
rmpath(path_test);
%=============================================================================
