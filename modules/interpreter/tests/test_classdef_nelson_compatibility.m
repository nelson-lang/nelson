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
%=============================================================================
asserts.isequal(parsefile([path_test, '/ClassdefAttributeSyntax.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefEnumWithConstructor.m']), 'classdef');
%=============================================================================
directOrigin = ClassdefPoint.origin();
asserts.isequal(class(directOrigin), 'ClassdefPoint');
asserts.isequal(directOrigin.X, 0);
asserts.isequal(directOrigin.Y, 0);
asserts.isequal(ClassdefPoint.Dimension, 2);
directColor = ClassdefColor.Red;
asserts.isequal(class(directColor), 'ClassdefColor');
asserts.isequal(directColor.Name, 'Red');
directEnumHigh = ClassdefEnumWithConstructor.High;
asserts.isequal(class(directEnumHigh), 'ClassdefEnumWithConstructor');
asserts.isequal(directEnumHigh.Name, 'High');
asserts.isequal(directEnumHigh.Code, 2);
% Enumeration members with arguments, whose class defines no constructor,
% forward the arguments to the inherited (superclass) constructor.
forwardLow = ClassdefEnumForwardingBase.Low;
asserts.isequal(class(forwardLow), 'ClassdefEnumForwardingBase');
asserts.istrue(isa(forwardLow, 'ClassdefEnumBaseWithCtor'));
asserts.isequal(forwardLow.A, 10);
asserts.isequal(forwardLow.B, 20);
asserts.isequal(char(forwardLow), 'Low');
forwardHigh = ClassdefEnumForwardingBase.High;
asserts.isequal(forwardHigh.A, 30);
asserts.isequal(forwardHigh.B, 40);
asserts.isequal(char(forwardHigh), 'High');
directPackaged = classdefpkg.ClassdefPackaged.origin();
asserts.isequal(class(directPackaged), 'classdefpkg.ClassdefPackaged');
asserts.isequal(directPackaged.Value, 0);
asserts.isequal(classdefpkg.ClassdefPackaged.ConstantValue, 12);
pointCell = {ClassdefPoint(3, 4)};
asserts.isequal(pointCell{1}.X, 3);
asserts.isequal(pointCell{1}.magnitude(), 5);
pointArray = ClassdefPoint(1, 2);
pointArray(end + 1, 1) = ClassdefPoint(5, 6);
asserts.isequal(numel(pointArray), 2);
asserts.isequal(pointArray(2).Y, 6);
%=============================================================================
target = ClassdefAccessTarget(19);
friend = ClassdefAccessFriend();
friendChild = ClassdefAccessFriendChild();
asserts.isequal(target.readFromDefiningClass(), 19);
asserts.isequal(friend.readFriendMethod(target), 19);
asserts.isequal(friend.readFriendDirectMethod(target), 20);
asserts.isequal(friend.readFriendProperty(target), 19);
asserts.isequal(friendChild.readAsChild(target), 19);
asserts.isequal(friendChild.readDirectAsChild(target), 20);
asserts.isequal(friendChild.readPropertyAsChild(target), 19);
target = friend.setFriendProperty(target, 23);
asserts.isequal(friend.readFriendProperty(target), 23);
asserts.isequal(friend.readFriendDirectMethod(target), 24);
target = friendChild.setPropertyAsChild(target, 31);
asserts.isequal(friendChild.readPropertyAsChild(target), 31);
asserts.isequal(friend.readFriendProperty(target), 31);
targetMethods = methods('ClassdefAccessTarget');
targetProperties = properties('ClassdefAccessTarget');
asserts.isfalse(any(strcmp(targetMethods, 'friendOnly')));
asserts.isfalse(any(strcmp(targetMethods, 'friendOnlyDirect')));
asserts.isfalse(any(strcmp(targetProperties, 'FriendValue')));
asserts.isfalse(ismethod(target, 'friendOnly'));
asserts.isfalse(ismethod(target, 'friendOnlyDirect'));
asserts.istrue(isprop(target, 'FriendValue'));
targetMeta = metaclass('ClassdefAccessTarget');
targetPropertyDetailNames = {targetMeta.PropertyDetails.Name};
friendValueDetail = targetMeta.PropertyDetails(find(strcmp(targetPropertyDetailNames, 'FriendValue')));
asserts.isequal(friendValueDetail.Access, '?ClassdefAccessFriend');
asserts.istrue(any(strcmp(friendValueDetail.Attributes, 'Access = ?ClassdefAccessFriend')));
targetMethodDetailNames = {targetMeta.MethodDetails.Name};
friendOnlyDetail = targetMeta.MethodDetails(find(strcmp(targetMethodDetailNames, 'friendOnly')));
asserts.isequal(friendOnlyDetail.Access, '{?ClassdefAccessFriend}');
asserts.istrue(any(strcmp(friendOnlyDetail.Attributes, 'Access = {?ClassdefAccessFriend}')));
friendOnlyDirectDetail = targetMeta.MethodDetails(find(strcmp(targetMethodDetailNames, 'friendOnlyDirect')));
asserts.isequal(friendOnlyDirectDetail.Access, '?ClassdefAccessFriend');
asserts.istrue(any(strcmp(friendOnlyDirectDetail.Attributes, 'Access = ?ClassdefAccessFriend')));
asserts.checkerror('target.FriendValue;', 'Cannot get property: FriendValue');
asserts.checkerror('target.FriendValue = 40;', 'Cannot set property: FriendValue');
try
  target.friendOnly();
  error('Access-list method call should fail outside an allowed class.');
catch M
  asserts.istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, _('Function not found.')) ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, _('Reference to non-existent field')) ...
    || contains(M.message, 'Undefined') ...
    || contains(M.message, _('Undefined variable or function:')));
end
try
  target.friendOnlyDirect();
  error('Direct metaclass access method call should fail outside an allowed class.');
catch M
  asserts.istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, _('Function not found.')) ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, _('Reference to non-existent field')) ...
    || contains(M.message, 'Undefined') ...
    || contains(M.message, _('Undefined variable or function:')));
end
%=============================================================================
listTarget = ClassdefAccessListTarget(50);
peer = ClassdefAccessPeer();
asserts.isequal(friend.readListProperty(listTarget), 50);
asserts.isequal(peer.readListProperty(listTarget), 50);
asserts.isequal(friend.readListMethod(listTarget), 51);
asserts.isequal(peer.readListMethod(listTarget), 51);
asserts.isequal(friend.readListConstant(), 13);
asserts.isequal(peer.readListConstant(), 13);
asserts.isequal(friend.readListStatic(), 15);
asserts.isequal(peer.readListStatic(), 15);
listTarget = peer.setListProperty(listTarget, 60);
asserts.isequal(peer.readListProperty(listTarget), 60);
asserts.isequal(friend.readListProperty(listTarget), 60);
listListener = friend.listenListEvent(listTarget);
peer.triggerListEvent(listTarget);
asserts.isequal(listTarget.LastEvent, 'SharedEvent');
delete(listListener);
listListener = peer.listenListEvent(listTarget);
peer.triggerListEvent(listTarget);
asserts.isequal(listTarget.LastEvent, 'SharedEvent');
delete(listListener);
asserts.checkerror('listTarget.SharedValue;', 'Cannot get property: SharedValue');
asserts.checkerror('listTarget.SharedValue = 70;', 'Cannot set property: SharedValue');
asserts.checkerror('addlistener(listTarget, ''SharedEvent'', @(src, eventData) src.recordEvent(eventData.EventName));', ...
  'Cannot listen event: SharedEvent');
asserts.checkerror('notify(listTarget, ''SharedEvent'');', 'Cannot notify event: SharedEvent');
try
  listTarget.sharedMethod();
  error('Access-list method call should fail outside an allowed class.');
catch M
  asserts.istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, _('Function not found.')) ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, _('Reference to non-existent field')) ...
    || contains(M.message, 'Undefined') ...
    || contains(M.message, _('Undefined variable or function:')));
end
try
  ClassdefAccessListTarget.SharedConstant;
  error('Access-list constant read should fail outside an allowed class.');
catch M
  asserts.istrue(contains(M.message, 'Cannot get property') ...
    || contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, _('Function not found.')) ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, _('Reference to non-existent field')) ...
    || contains(M.message, 'Undefined') ...
    || contains(M.message, _('Undefined variable or function:')));
end
try
  ClassdefAccessListTarget.sharedStatic();
  error('Access-list static method call should fail outside an allowed class.');
catch M
  asserts.istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, _('Function not found.')) ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, _('Reference to non-existent field')) ...
    || contains(M.message, 'Undefined') ...
    || contains(M.message, _('Undefined variable or function:')));
end
listVisibleProperties = properties('ClassdefAccessListTarget');
asserts.isfalse(any(strcmp(listVisibleProperties, 'SharedValue')));
asserts.isfalse(any(strcmp(listVisibleProperties, 'SharedConstant')));
listVisibleMethods = methods('ClassdefAccessListTarget');
asserts.isfalse(any(strcmp(listVisibleMethods, 'sharedMethod')));
asserts.isfalse(any(strcmp(listVisibleMethods, 'sharedStatic')));
listMeta = metaclass('ClassdefAccessListTarget');
listPropertyNames = {listMeta.PropertyDetails.Name};
sharedValueDetail = listMeta.PropertyDetails(find(strcmp(listPropertyNames, 'SharedValue')));
asserts.isequal(sharedValueDetail.GetAccess, '{?ClassdefAccessFriend, ?ClassdefAccessPeer}');
asserts.isequal(sharedValueDetail.SetAccess, '?ClassdefAccessPeer');
sharedConstantDetail = listMeta.PropertyDetails(find(strcmp(listPropertyNames, 'SharedConstant')));
asserts.istrue(sharedConstantDetail.Constant);
asserts.isequal(sharedConstantDetail.GetAccess, '{?ClassdefAccessFriend, ?ClassdefAccessPeer}');
listMethodNames = {listMeta.MethodDetails.Name};
sharedMethodDetail = listMeta.MethodDetails(find(strcmp(listMethodNames, 'sharedMethod')));
asserts.isequal(sharedMethodDetail.Access, '{?ClassdefAccessFriend, ?ClassdefAccessPeer}');
sharedStaticDetail = listMeta.MethodDetails(find(strcmp(listMethodNames, 'sharedStatic')));
asserts.istrue(sharedStaticDetail.Static);
asserts.isequal(sharedStaticDetail.Access, '{?ClassdefAccessFriend, ?ClassdefAccessPeer}');
listEventNames = {listMeta.EventDetails.Name};
sharedEventDetail = listMeta.EventDetails(find(strcmp(listEventNames, 'SharedEvent')));
asserts.isequal(sharedEventDetail.ListenAccess, '{?ClassdefAccessFriend, ?ClassdefAccessPeer}');
asserts.isequal(sharedEventDetail.NotifyAccess, '?ClassdefAccessPeer');
%=============================================================================
points = [ClassdefPoint(1, 2), ClassdefPoint(3, 4)];
asserts.isequal(class(points), 'ClassdefPoint');
asserts.isequal(length(points), 2);
asserts.isequal(points(1).X, 1);
asserts.isequal(points(2).Y, 4);
points(2).X = 99;
points(1).Label = 'first';
asserts.isequal(points(1).Label, 'first');
asserts.isequal(points(2).X, 99);
points(2) = points(2).shift(1, 2);
asserts.isequal(points(2).X, 100);
asserts.isequal(points(2).Y, 6);
points(2) = ClassdefPoint(13, 24);
asserts.isequal(points(2).X, 13);
asserts.isequal(points(2).Y, 24);
points([1, 2]) = [ClassdefPoint(5, 6), ClassdefPoint(7, 8)];
asserts.isequal(points(1).X, 5);
asserts.isequal(points(2).Y, 8);
expandedPoints(3) = ClassdefPoint(11, 12);
asserts.isequal(class(expandedPoints), 'ClassdefPoint');
asserts.isequal(size(expandedPoints), [1 3]);
asserts.isequal(expandedPoints(1).X, 0);
asserts.isequal(expandedPoints(1).Label, 'point');
asserts.isequal(expandedPoints(2).Y, 0);
asserts.isequal(expandedPoints(2).Label, 'point');
asserts.isequal(expandedPoints(3).X, 11);
asserts.isequal(expandedPoints(3).Y, 12);
seededPoints = ClassdefPoint(21, 22);
seededPoints(3) = ClassdefPoint(31, 32);
asserts.isequal(seededPoints(1).X, 21);
asserts.isequal(seededPoints(2).X, 0);
asserts.isequal(seededPoints(2).Label, 'point');
asserts.isequal(seededPoints(3).Y, 32);
gridPoints(2, 3) = ClassdefPoint(41, 42);
asserts.isequal(class(gridPoints), 'ClassdefPoint');
asserts.isequal(size(gridPoints), [2 3]);
asserts.isequal(gridPoints(1, 1).Label, 'point');
asserts.isequal(gridPoints(2, 3).X, 41);
try
  points(1) = ClassdefDerived(9);
  error('Wrong class assignment should fail.');
catch M
  asserts.istrue(contains(M.message, 'Cannot promote to another class type array') ...
    || contains(M.message, _('Cannot promote to another class type array.')) ...
    || contains(M.message, 'Invalid indexing') ...
    || contains(M.message, _('Invalid indexing.')));
end
dependentValue = ClassdefDependentValue(6);
asserts.isequal(dependentValue.Base, 6);
asserts.isequal(dependentValue.Twice, 12);
dependentValue.Twice = 40;
asserts.isequal(dependentValue.Base, 20);
asserts.isequal(dependentValue.Twice, 40);
asserts.isequal(dependentValue.ReadOnlyTwice, 40);
dependentValue = dependentValue.setReadOnlyTwiceInside(50);
asserts.isequal(dependentValue.Base, 25);
asserts.checkerror('dependentValue.ReadOnlyTwice = 60;', 'Cannot set property: ReadOnlyTwice');
dependentProperties = properties(dependentValue);
asserts.istrue(any(strcmp(dependentProperties, 'Twice')));
asserts.istrue(any(strcmp(dependentProperties, 'ReadOnlyTwice')));
dependentMethods = methods(dependentValue);
asserts.isfalse(any(strcmp(dependentMethods, 'get.Twice')));
asserts.isfalse(any(strcmp(dependentMethods, 'set.Twice')));
dependentMeta = metaclass(dependentValue);
asserts.istrue(any(strcmp({dependentMeta.PropertyList.Name}, 'Twice')));
dependentPropertyDetailNames = {dependentMeta.PropertyDetails.Name};
twiceDetail = dependentMeta.PropertyDetails(find(strcmp(dependentPropertyDetailNames, 'Twice')));
asserts.istrue(twiceDetail.Dependent);
readOnlyTwiceDetail = dependentMeta.PropertyDetails(find(strcmp(dependentPropertyDetailNames, 'ReadOnlyTwice')));
asserts.istrue(readOnlyTwiceDetail.Dependent);
asserts.isequal(readOnlyTwiceDetail.SetAccess, 'private');
dependentArray = [ClassdefDependentValue(2), ClassdefDependentValue(4)];
dependentTwice = [dependentArray.Twice];
asserts.isequal(dependentTwice, [4, 8]);
dependentHandle = ClassdefDependentHandle(7);
asserts.isequal(dependentHandle.Twice, 14);
dependentHandle.Twice = 30;
asserts.isequal(dependentHandle.Base, 15);
delete(dependentHandle);
rmpath(path_test);
%=============================================================================
