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
assert_isequal(parsefile([path_test, '/ClassdefPoint.m']), 'classdef');
%=============================================================================
p = ClassdefPoint(3, 4);
assert_isequal(class(p), 'ClassdefPoint');
assert_istrue(isa(p, 'ClassdefPoint'));
assert_istrue(isobject(p));
assert_isequal(p.X, 3);
assert_isequal(p.Y, 4);
assert_isequal(p.Label, 'point');
assert_isequal(p.Dimension, 2);
assert_checkerror('p.Dimension = 3;', 'Cannot modify constant property: Dimension');
pointStruct = struct(p);
assert_isequal(pointStruct.X, 3);
assert_isequal(pointStruct.Y, 4);
persistPoint = ClassdefPoint(6, 8);
persistPointFile = [tempdir(), 'classdef_point_roundtrip.nh5'];
save(persistPointFile, 'persistPoint');
clear persistPoint;
load(persistPointFile);
assert_isequal(class(persistPoint), 'ClassdefPoint');
assert_isequal(persistPoint.X, 6);
assert_isequal(persistPoint.Y, 8);
persistPointDefault = ClassdefPoint(9, 12);
persistPointDefaultBase = [tempdir(), 'classdef_point_default_roundtrip'];
persistPointDefaultFile = [persistPointDefaultBase, '.nh5'];
if isfile(persistPointDefaultFile)
  rmfile(persistPointDefaultFile);
end
save(persistPointDefaultBase, 'persistPointDefault');
clear persistPointDefault;
load(persistPointDefaultBase);
assert_isequal(class(persistPointDefault), 'ClassdefPoint');
assert_isequal(persistPointDefault.X, 9);
assert_isequal(persistPointDefault.Y, 12);
%=============================================================================
assert_isequal(magnitude(p), 5);
profile('clear');
profile('on');
profPoint = ClassdefPoint(3, 4);
magnitude(profPoint);
profile('off');
profInfo = profile('info');
classdefPointFile = [path_test, '/ClassdefPoint.m'];
foundSqrtAtMethodLine = false;
for k = 1:length(profInfo)
  if strcmp(profInfo(k).FunctionName, 'sqrt') ...
      && strcmp(profInfo(k).Filename, classdefPointFile) ...
      && profInfo(k).LinePosition == 25
    foundSqrtAtMethodLine = true;
  end
end
assert_istrue(foundSqrtAtMethodLine);
profile('clear');
dbclear all;
dbstop in ClassdefPoint at 25;
bp = dbstatus();
assert_isequal(bp.name, 'ClassdefPoint');
assert_istrue(isfile(bp.file));
assert_isequal(bp.line, 25);
dbclear all;
q = p.shift(1, 2);
assert_isequal(q.X, 4);
assert_isequal(q.Y, 6);
assert_isequal(p.X, 3);
assert_isequal(p.Y, 4);
%=============================================================================
origin = useClassdefPointStatic();
assert_isequal(class(origin), 'ClassdefPoint');
assert_isequal(origin.X, 0);
assert_isequal(origin.Y, 0);
assert_isequal(useClassdefPointConstant(), 2);
%=============================================================================
m = methods('ClassdefPoint');
assert_istrue(any(strcmp(m, 'magnitude')));
assert_istrue(any(strcmp(m, 'origin')));
assert_istrue(any(strcmp(m, 'shift')));
props = properties(p);
assert_istrue(any(strcmp(props, 'X')));
assert_istrue(any(strcmp(props, 'Y')));
assert_istrue(any(strcmp(props, 'Label')));
assert_istrue(any(strcmp(props, 'Dimension')));
assert_istrue(ismethod(p, 'magnitude'));
assert_istrue(ismethod('ClassdefPoint', 'origin'));
assert_isfalse(ismethod(p, 'missingMethod'));
assert_istrue(isprop(p, 'X'));
assert_isfalse(isprop(p, 'missingProperty'));
pointArray = [p, q];
assert_isequal(isprop(pointArray, 'X'), [true, true]);
assert_isequal(isprop(pointArray, 'missingProperty'), [false, false]);
assert_isequal(fieldnames(p), props);
pointEvents = events(p);
assert_istrue(any(strcmp(pointEvents, 'Moved')));
assert_isequal(events('ClassdefPoint'), pointEvents);
assert_isequal(parsestring('m = ?ClassdefPoint;'), 'script');
pointMeta = metaclass('ClassdefPoint');
pointMetaFromQuery = ?ClassdefPoint;
assert_isequal(pointMetaFromQuery.Name, pointMeta.Name);
assert_istrue(isfield(pointMeta, 'PropertyDetails'));
assert_istrue(isfield(pointMeta, 'MethodDetails'));
assert_istrue(isfield(pointMeta, 'EventDetails'));
assert_istrue(isfield(pointMeta, 'EnumerationDetails'));
assert_istrue(isfield(pointMeta, 'ClassDetails'));
assert_isequal(pointMeta.ClassDetails.Name, 'ClassdefPoint');
assert_isfalse(pointMeta.ClassDetails.Abstract);
assert_isfalse(pointMeta.ClassDetails.Sealed);
assert_isfalse(pointMeta.ClassDetails.Handle);
pointPropertyDetailNames = {pointMeta.PropertyDetails.Name};
assert_istrue(any(strcmp(pointPropertyDetailNames, 'Dimension')));
dimensionDetail = pointMeta.PropertyDetails(find(strcmp(pointPropertyDetailNames, 'Dimension')));
assert_istrue(dimensionDetail.Constant);
assert_isequal(dimensionDetail.GetAccess, 'public');
assert_isequal(dimensionDetail.SetAccess, 'public');
assert_istrue(any(strcmp(dimensionDetail.Attributes, 'Constant')));
pointMethodDetailNames = {pointMeta.MethodDetails.Name};
assert_istrue(any(strcmp(pointMethodDetailNames, 'origin')));
originDetail = pointMeta.MethodDetails(find(strcmp(pointMethodDetailNames, 'origin')));
assert_istrue(originDetail.Static);
assert_isequal(originDetail.Access, 'public');
assert_istrue(any(strcmp(originDetail.Attributes, 'Static')));
pointEventDetailNames = {pointMeta.EventDetails.Name};
movedEventDetail = pointMeta.EventDetails(find(strcmp(pointEventDetailNames, 'Moved')));
assert_isequal(movedEventDetail.DefiningClass, 'ClassdefPoint');
assert_checkerror('missingMeta = ?ClassdefMissing;', 'Class not found: ClassdefMissing');
%=============================================================================
d = ClassdefDerived(5);
assert_isequal(class(d), 'ClassdefDerived');
assert_istrue(isa(d, 'ClassdefDerived'));
assert_istrue(isa(d, 'ClassdefBase'));
assert_isfalse(isa(d, 'ClassdefPoint'));
assert_isequal(d.BaseValue, 10);
assert_isequal(d.BaseConstant, 7);
assert_isequal(d.DerivedValue, 5);
assert_isequal(baseMethod(d), 10);
assert_isequal(d.baseMethod(), 10);
assert_isequal(d.derivedMethod(), 15);
assert_isequal(sealedMethod(d), 'sealed');
assert_isequal(d.sealedMethod(), 'sealed');
assert_isequal(d.hiddenMethod(), 99);
assert_isequal(d.callPrivate(), -2);
assert_isequal(d.callProtected(), 42);
assert_isequal(d.cannotCallBasePrivate(), 'blocked');
assert_isequal(d.readPrivateProperty(), 31);
d = d.setPrivateProperty(32);
assert_isequal(d.readPrivateProperty(), 32);
assert_isequal(d.readBaseProtectedProperty(), 41);
assert_isequal(d.cannotReadBasePrivateProperty(), 'blocked');
assert_isequal(d.BaseReadOnlyByAccess, 51);
d = d.setReadOnlyProperty(52);
assert_isequal(d.BaseReadOnlyByAccess, 52);
assert_checkerror('d.BaseReadOnlyByAccess = 53;', 'Cannot set property: BaseReadOnlyByAccess');
d.BaseWriteOnlyPublic = 62;
assert_isequal(d.readWriteOnlyProperty(), 62);
assert_checkerror('d.BaseWriteOnlyPublic;', 'Cannot get property: BaseWriteOnlyPublic');
assert_isequal(useClassdefDerivedInheritedStatic(), 'base');
assert_isequal(useClassdefDerivedInheritedConstant(), 7);
assert_isequal(useClassdefBasePrivateStatic(), 'privateStatic');
assert_isequal(useClassdefDerivedProtectedStatic(), 'protectedStatic');
assert_isequal(useClassdefDerivedPrivateStaticBlocked(), 'blocked');
derivedMethods = methods('ClassdefDerived');
assert_istrue(any(strcmp(derivedMethods, 'baseMethod')));
assert_istrue(any(strcmp(derivedMethods, 'baseStatic')));
assert_istrue(any(strcmp(derivedMethods, 'derivedMethod')));
assert_istrue(any(strcmp(derivedMethods, 'callPrivate')));
assert_istrue(any(strcmp(derivedMethods, 'callProtected')));
assert_istrue(any(strcmp(derivedMethods, 'readPrivateProperty')));
assert_istrue(any(strcmp(derivedMethods, 'readBaseProtectedProperty')));
assert_istrue(any(strcmp(derivedMethods, 'sealedMethod')));
assert_isfalse(any(strcmp(derivedMethods, 'hiddenMethod')));
assert_isfalse(any(strcmp(derivedMethods, 'privateMethod')));
assert_isfalse(any(strcmp(derivedMethods, 'protectedMethod')));
assert_isfalse(ismethod(d, 'hiddenMethod'));
assert_isfalse(ismethod(d, 'privateMethod'));
assert_isfalse(ismethod(d, 'protectedMethod'));
try
  d.privateMethod();
  error('private method call should fail');
catch M
  assert_istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, 'Undefined'));
end
try
  privateMethod(d);
  error('private method call should fail');
catch M
  assert_istrue(contains(M.message, 'Function not found') ...
    || contains(M.message, 'Undefined variable or function') ...
    || contains(M.message, 'undefined.'));
end
try
  d.protectedMethod();
  error('protected method call should fail');
catch M
  assert_istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, 'Undefined'));
end
try
  protectedMethod(d);
  error('protected method call should fail');
catch M
  assert_istrue(contains(M.message, 'Function not found') ...
    || contains(M.message, 'Undefined variable or function') ...
    || contains(M.message, 'undefined.'));
end
assert_checkerror('d.BasePrivateValue;', 'Cannot get property: BasePrivateValue');
assert_checkerror('d.BasePrivateValue = 33;', 'Cannot set property: BasePrivateValue');
assert_checkerror('d.BaseProtectedValue;', 'Cannot get property: BaseProtectedValue');
assert_checkerror('d.BaseProtectedValue = 42;', 'Cannot set property: BaseProtectedValue');
try
  useClassdefBasePrivateStaticExternal();
  error('private static method call should fail');
catch M
  assert_istrue(contains(M.message, 'Undefined variable or function') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, 'Cannot get property') ...
    || contains(M.message, 'No such property'));
end
try
  useClassdefBaseProtectedStaticExternal();
  error('protected static method call should fail');
catch M
  assert_istrue(contains(M.message, 'Undefined variable or function') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, 'Cannot get property') ...
    || contains(M.message, 'No such property'));
end
derivedProps = properties(d);
assert_istrue(any(strcmp(derivedProps, 'BaseValue')));
assert_istrue(any(strcmp(derivedProps, 'BaseConstant')));
assert_istrue(any(strcmp(derivedProps, 'BaseReadOnlyByAccess')));
assert_istrue(any(strcmp(derivedProps, 'DerivedValue')));
assert_isfalse(any(strcmp(derivedProps, 'BasePrivateValue')));
assert_isfalse(any(strcmp(derivedProps, 'BaseProtectedValue')));
assert_isfalse(any(strcmp(derivedProps, 'BaseWriteOnlyPublic')));
derivedEvents = events(d);
assert_istrue(any(strcmp(derivedEvents, 'BaseChanged')));
assert_checkerror('d.BaseConstant = 8;', 'Cannot modify constant property: BaseConstant');
assert_checkerror('ClassdefAbstract()', 'Cannot instantiate abstract class: ClassdefAbstract');
assert_checkerror('ClassdefSealedOverride()', 'Cannot override sealed method: sealedMethod');
assert_checkerror('ClassdefSealedSubclass()', 'Cannot inherit from sealed class: ClassdefSealedClass');
abstractMeta = metaclass('ClassdefAbstract');
assert_istrue(abstractMeta.ClassDetails.Abstract);
assert_istrue(any(strcmp(abstractMeta.ClassDetails.Attributes, 'Abstract')));
sealedClassMeta = metaclass('ClassdefSealedClass');
assert_istrue(sealedClassMeta.ClassDetails.Sealed);
assert_istrue(any(strcmp(sealedClassMeta.ClassDetails.Attributes, 'Sealed')));
%=============================================================================
splitObj = ClassdefSplit();
assert_isequal(class(splitObj), 'ClassdefSplit');
assert_isequal(externalValue(splitObj), 12);
assert_isequal(splitObj.externalValue(), 12);
assert_isequal(splitObj.callPrivateExternal(), 8);
assert_isequal(useClassdefSplitStaticExternal(), 'splitStatic');
assert_isequal(useClassdefSplitPrivateExternal(), 'blocked');
splitDerived = ClassdefSplitDerived();
assert_istrue(isa(splitDerived, 'ClassdefSplit'));
assert_isequal(splitDerived.callProtectedExternal(), 16);
assert_checkerror('splitObj.privateExternal();', 'No such property: privateExternal');
assert_checkerror('splitObj.protectedExternal();', 'No such property: protectedExternal');
assert_istrue(ismethod(splitObj, 'externalValue'));
assert_isfalse(ismethod(splitObj, 'privateExternal'));
assert_isfalse(ismethod(splitObj, 'protectedExternal'));
splitMethods = methods('ClassdefSplit');
assert_istrue(any(strcmp(splitMethods, 'externalValue')));
assert_istrue(any(strcmp(splitMethods, 'callPrivateExternal')));
assert_istrue(any(strcmp(splitMethods, 'staticExternal')));
assert_isfalse(any(strcmp(splitMethods, 'privateExternal')));
assert_isfalse(any(strcmp(splitMethods, 'protectedExternal')));
splitMeta = metaclass('ClassdefSplit');
splitMethodDetailNames = {splitMeta.MethodDetails.Name};
staticExternalDetail = splitMeta.MethodDetails(find(strcmp(splitMethodDetailNames, 'staticExternal')));
assert_istrue(staticExternalDetail.Static);
privateExternalDetail = splitMeta.MethodDetails(find(strcmp(splitMethodDetailNames, 'privateExternal')));
assert_isequal(privateExternalDetail.Access, 'private');
protectedExternalDetail = splitMeta.MethodDetails(find(strcmp(splitMethodDetailNames, 'protectedExternal')));
assert_isequal(protectedExternalDetail.Access, 'protected');
%=============================================================================
packaged = useClassdefPackagedConstructor(6);
assert_isequal(class(packaged), 'classdefpkg.ClassdefPackaged');
assert_istrue(isa(packaged, 'classdefpkg.ClassdefPackaged'));
assert_isequal(packaged.Value, 6);
assert_isequal(packaged.triple(), 18);
assert_isequal(useClassdefPackagedConstant(), 12);
packagedOrigin = useClassdefPackagedStatic();
assert_isequal(class(packagedOrigin), 'classdefpkg.ClassdefPackaged');
assert_isequal(packagedOrigin.Value, 0);
packagedMethods = methods('classdefpkg.ClassdefPackaged');
assert_istrue(any(strcmp(packagedMethods, 'triple')));
assert_istrue(any(strcmp(packagedMethods, 'origin')));
packagedProps = properties(packaged);
assert_istrue(any(strcmp(packagedProps, 'Value')));
assert_istrue(any(strcmp(packagedProps, 'ConstantValue')));
packagedMeta = metaclass('classdefpkg.ClassdefPackaged');
assert_isequal(packagedMeta.Name, 'classdefpkg.ClassdefPackaged');
packagedMetaFromQuery = ?classdefpkg.ClassdefPackaged;
assert_isequal(packagedMetaFromQuery.Name, 'classdefpkg.ClassdefPackaged');
%=============================================================================
counter = ClassdefCounter(2);
assert_isequal(class(counter), 'ClassdefCounter');
assert_istrue(ishandle(counter));
assert_istrue(isobject(counter));
assert_istrue(isvalid(counter));
assert_istrue(isa(counter, 'ClassdefCounter'));
assert_istrue(isa(counter, 'handle'));
assert_isequal(counter.Count, 2);
assert_isequal(counter.secretCount(), -5);
counter.setSecretCount(-6);
assert_isequal(counter.secretCount(), -6);
assert_isequal(counter.protectedCount(), 100);
assert_isequal(counter.ReadOnlyCount, 77);
counter.setReadOnlyCount(78);
assert_isequal(counter.ReadOnlyCount, 78);
assert_checkerror('counter.ReadOnlyCount = 79;', 'Cannot set property: ReadOnlyCount');
counter.WriteOnlyCount = 89;
assert_isequal(counter.writeOnlyCount(), 89);
assert_checkerror('counter.WriteOnlyCount;', 'Cannot get property: WriteOnlyCount');
assert_checkerror('counter.SecretCount;', 'Cannot get property: SecretCount');
assert_checkerror('counter.SecretCount = -7;', 'Cannot set property: SecretCount');
assert_checkerror('counter.ProtectedCount;', 'Cannot get property: ProtectedCount');
assert_checkerror('counter.ProtectedCount = 101;', 'Cannot set property: ProtectedCount');
sameCounter = counter;
sameCounter.increment(3);
assert_isequal(counter.Count, 5);
sameCounter.Count = 9;
assert_isequal(counter.Count, 9);
assert_isequal(counter.LastEvent, '');
counterStruct = struct(counter);
assert_isequal(counterStruct.Count, 9);
assert_isequal(fieldnames(counter), properties(counter));
counterProps = properties(counter);
assert_istrue(any(strcmp(counterProps, 'ReadOnlyCount')));
assert_isfalse(any(strcmp(counterProps, 'SecretCount')));
assert_isfalse(any(strcmp(counterProps, 'ProtectedCount')));
assert_isfalse(any(strcmp(counterProps, 'WriteOnlyCount')));
assert_istrue(isprop(counter, 'Count'));
assert_istrue(ismethod(counter, 'increment'));
counterEvents = events(counter);
assert_istrue(any(strcmp(counterEvents, 'CountChanged')));
assert_istrue(any(strcmp(counterEvents, 'ObjectBeingDestroyed')));
counterArrayA = ClassdefCounter(12);
counterArrayB = ClassdefCounter(13);
counterArray = [counterArrayA, counterArrayB];
counterArrayProps = properties(counterArray);
assert_istrue(any(strcmp(counterArrayProps, 'Count')));
assert_isfalse(any(strcmp(counterArrayProps, 'SecretCount')));
counterArrayMethods = methods(counterArray);
assert_istrue(any(strcmp(counterArrayMethods, 'increment')));
assert_istrue(any(strcmp(fieldnames(counterArray), 'Count')));
assert_istrue(any(strcmp(events(counterArray), 'CountChanged')));
assert_istrue(ismethod(counterArray, 'increment'));
assert_isfalse(ismethod(counterArray, 'missingMethod'));
assert_isequal(isprop(counterArray, 'Count'), [true, true]);
assert_isequal(isprop(counterArray, 'SecretCount'), [true, true]);
assert_isequal(isprop(counterArray, 'missingProperty'), [false, false]);
delete(counterArray);
counterListener = addlistener(counter, 'CountChanged', @(src, eventData) src.recordEvent(eventData.EventName));
assert_isequal(class(counterListener), 'event.listener');
assert_istrue(ishandle(counterListener));
assert_istrue(isvalid(counterListener));
counter.increment(1);
assert_isequal(counter.Count, 10);
assert_isequal(counter.LastEvent, 'CountChanged');
delete(counterListener);
assert_isfalse(isvalid(counterListener));
counter.LastEvent = '';
counter.increment(1);
assert_isequal(counter.Count, 11);
assert_isequal(counter.LastEvent, '');
customListener = addlistener(counter, 'CountChanged', @(src, eventData) src.recordEvent(eventData.Name));
notify(counter, 'CountChanged', struct('Name', 'custom'));
assert_isequal(counter.LastEvent, 'custom');
delete(customListener);
aliasListener = listener(counter, 'CountChanged', @(src, eventData) src.recordEvent('alias'));
notify(counter, 'CountChanged');
assert_isequal(counter.LastEvent, 'alias');
delete(aliasListener);
observable = ClassdefObservableCounter(3);
postSetListener = addlistener(observable, 'Count', 'PostSet', @(src, eventData) src.recordPropertyEvent(eventData));
observable.Count = 4;
assert_isequal(observable.Count, 4);
assert_isequal(observable.LastEvent, 'PostSet');
assert_isequal(observable.LastProperty, 'Count');
delete(postSetListener);
preSetListener = addlistener(observable, 'Count', 'PreSet', @(src, eventData) src.recordPropertyEvent(eventData));
observable.LastEvent = '';
observable.Count = 5;
assert_isequal(observable.LastEvent, 'PreSet');
assert_isequal(observable.LastProperty, 'Count');
delete(preSetListener);
preGetListener = addlistener(observable, 'Count', 'PreGet', @(src, eventData) src.recordPropertyEvent(eventData));
observable.LastEvent = '';
value = observable.Count;
assert_isequal(value, 5);
assert_isequal(observable.LastEvent, 'PreGet');
assert_isequal(observable.LastProperty, 'Count');
delete(preGetListener);
postGetListener = listener(observable, 'Count', 'PostGet', @(src, eventData) src.recordPropertyEvent(eventData));
observable.LastEvent = '';
value = observable.Count;
assert_isequal(value, 5);
assert_isequal(observable.LastEvent, 'PostGet');
assert_isequal(observable.LastProperty, 'Count');
delete(postGetListener);
assert_checkerror('addlistener(observable, ''LastEvent'', ''PostSet'', @(src, eventData) disp(eventData.EventName));', 'No such property event: LastEvent.PostSet');
global CLASSDEF_COUNTER_DESTROY_STATE_LOG;
CLASSDEF_COUNTER_DESTROY_STATE_LOG = '';
observableDestroyListener = addlistener(observable, 'ObjectBeingDestroyed', @classdefCounterDestroyStateCallback);
delete(observable);
assert_isfalse(isvalid(observable));
assert_isfalse(isvalid(observableDestroyListener));
assert_isequal(CLASSDEF_COUNTER_DESTROY_STATE_LOG, 'ObjectBeingDestroyed:valid:5');
zeroCounter = useClassdefCounterStatic();
assert_isequal(class(zeroCounter), 'ClassdefCounter');
assert_isequal(zeroCounter.Count, 0);
counterMeta = metaclass(counter);
assert_isequal(counterMeta.Name, 'ClassdefCounter');
assert_istrue(counterMeta.ClassDetails.Handle);
assert_istrue(any(strcmp(counterMeta.SuperclassList, 'handle')));
counterEventDetailNames = {counterMeta.EventDetails.Name};
destroyedEventDetail = counterMeta.EventDetails(find(strcmp(counterEventDetailNames, 'ObjectBeingDestroyed')));
assert_isequal(destroyedEventDetail.DefiningClass, 'handle');
observableMeta = metaclass('ClassdefObservableCounter');
observablePropertyDetailNames = {observableMeta.PropertyDetails.Name};
assert_istrue(any(strcmp(observablePropertyDetailNames, 'Count')));
observableCountDetail = observableMeta.PropertyDetails(find(strcmp(observablePropertyDetailNames, 'Count')));
assert_istrue(observableCountDetail.GetObservable);
assert_istrue(observableCountDetail.SetObservable);
assert_istrue(any(strcmp(observableCountDetail.Attributes, 'GetObservable')));
assert_istrue(any(strcmp(observableCountDetail.Attributes, 'SetObservable')));
persistCounter = ClassdefCounter(4);
persistCounterFile = [tempdir(), 'classdef_counter_roundtrip.nh5'];
save(persistCounterFile, 'persistCounter');
delete(persistCounter);
clear persistCounter;
load(persistCounterFile);
assert_isequal(class(persistCounter), 'ClassdefCounter');
assert_istrue(ishandle(persistCounter));
assert_istrue(isvalid(persistCounter));
assert_isequal(persistCounter.Count, 4);
persistCounter.increment(2);
assert_isequal(persistCounter.Count, 6);
delete(persistCounter);
global CLASSDEF_COUNTER_DELETE_LOG;
CLASSDEF_COUNTER_DELETE_LOG = '';
CLASSDEF_COUNTER_DESTROY_STATE_LOG = '';
stateCounter = ClassdefCounter(8);
stateListener = addlistener(stateCounter, 'ObjectBeingDestroyed', @classdefCounterDestroyStateCallback);
delete(stateCounter);
assert_isfalse(isvalid(stateCounter));
assert_isfalse(isvalid(stateListener));
assert_isequal(CLASSDEF_COUNTER_DELETE_LOG, 'delete');
assert_isequal(CLASSDEF_COUNTER_DESTROY_STATE_LOG, 'ObjectBeingDestroyed:valid:8');
CLASSDEF_COUNTER_DELETE_LOG = '';
destroyCounter = ClassdefCounter(1);
destroyListener = addlistener(destroyCounter, 'ObjectBeingDestroyed', @classdefCounterDestroyCallback);
delete(destroyCounter);
assert_isfalse(isvalid(destroyCounter));
assert_isfalse(isvalid(destroyListener));
assert_isequal(CLASSDEF_COUNTER_DELETE_LOG, 'ObjectBeingDestroyeddelete');
CLASSDEF_COUNTER_DELETE_LOG = '';
destroyCounterA = ClassdefCounter(2);
destroyCounterB = ClassdefCounter(3);
destroyListenerA = addlistener(destroyCounterA, 'ObjectBeingDestroyed', @classdefCounterDestroyCallback);
destroyListenerB = addlistener(destroyCounterB, 'ObjectBeingDestroyed', @classdefCounterDestroyCallback);
destroyCounters = [destroyCounterA, destroyCounterB];
delete(destroyCounters);
assert_isfalse(isvalid(destroyCounterA));
assert_isfalse(isvalid(destroyCounterB));
assert_isfalse(isvalid(destroyListenerA));
assert_isfalse(isvalid(destroyListenerB));
assert_isequal(CLASSDEF_COUNTER_DELETE_LOG, 'ObjectBeingDestroyeddeleteObjectBeingDestroyeddelete');
listenerCounter = ClassdefCounter(4);
listenerA = addlistener(listenerCounter, 'CountChanged', @(src, eventData) src.recordEvent('first'));
listenerB = addlistener(listenerCounter, 'CountChanged', @(src, eventData) src.recordEvent('second'));
delete([listenerA, listenerB]);
assert_isfalse(isvalid(listenerA));
assert_isfalse(isvalid(listenerB));
listenerCounter.LastEvent = '';
notify(listenerCounter, 'CountChanged');
assert_isequal(listenerCounter.LastEvent, '');
delete(listenerCounter);
delete(counter);
assert_isfalse(isvalid(counter));
delete(zeroCounter);
clear CLASSDEF_COUNTER_DELETE_LOG;
clear CLASSDEF_COUNTER_DESTROY_STATE_LOG;
%=============================================================================
red = useClassdefColorRed();
blue = useClassdefColorBlue();
assert_isequal(class(red), 'ClassdefColor');
assert_istrue(isa(red, 'ClassdefColor'));
assert_isequal(red.Name, 'Red');
assert_isequal(blue.Name, 'Blue');
assert_istrue(useClassdefColorEquality());
assert_istrue(useClassdefColorInequality());
assert_istrue(red.isRed());
assert_isequal(char(red), 'Red');
persistColor = red;
persistColorFile = [tempdir(), 'classdef_color_roundtrip.nh5'];
save(persistColorFile, 'persistColor');
clear persistColor;
load(persistColorFile);
assert_isequal(class(persistColor), 'ClassdefColor');
assert_isequal(persistColor.Name, 'Red');
assert_istrue(persistColor.isRed());
enumMembers = enumeration('ClassdefColor');
assert_istrue(any(strcmp(enumMembers, 'Red')));
assert_istrue(any(strcmp(enumMembers, 'Blue')));
colorMeta = metaclass(red);
assert_isequal(colorMeta.Name, 'ClassdefColor');
assert_istrue(any(strcmp(colorMeta.EnumerationMemberList, 'Red')));
colorMetaFromQuery = ?ClassdefColor;
assert_isequal(colorMetaFromQuery.Name, 'ClassdefColor');
colorEnumerationDetailNames = {colorMeta.EnumerationDetails.Name};
redDetail = colorMeta.EnumerationDetails(find(strcmp(colorEnumerationDetailNames, 'Red')));
assert_isequal(redDetail.DefiningClass, 'ClassdefColor');
derivedMeta = metaclass('ClassdefDerived');
assert_isequal(derivedMeta.Name, 'ClassdefDerived');
assert_istrue(any(strcmp(derivedMeta.SuperclassList, 'ClassdefBase')));
assert_istrue(any(strcmp(derivedMeta.PropertyList, 'BaseValue')));
assert_istrue(any(strcmp(derivedMeta.MethodList, 'derivedMethod')));
assert_istrue(any(strcmp(derivedMeta.EventList, 'BaseChanged')));
derivedPropertyDetailNames = {derivedMeta.PropertyDetails.Name};
basePrivateDetail = derivedMeta.PropertyDetails(find(strcmp(derivedPropertyDetailNames, 'BasePrivateValue')));
assert_isequal(basePrivateDetail.Access, 'private');
assert_isequal(basePrivateDetail.GetAccess, 'private');
baseReadOnlyDetail = derivedMeta.PropertyDetails(find(strcmp(derivedPropertyDetailNames, 'BaseReadOnlyByAccess')));
assert_isequal(baseReadOnlyDetail.GetAccess, 'public');
assert_isequal(baseReadOnlyDetail.SetAccess, 'private');
derivedMethodDetailNames = {derivedMeta.MethodDetails.Name};
baseStaticDetail = derivedMeta.MethodDetails(find(strcmp(derivedMethodDetailNames, 'baseStatic')));
assert_istrue(baseStaticDetail.Static);
hiddenMethodDetail = derivedMeta.MethodDetails(find(strcmp(derivedMethodDetailNames, 'hiddenMethod')));
assert_istrue(hiddenMethodDetail.Hidden);
privateMethodDetail = derivedMeta.MethodDetails(find(strcmp(derivedMethodDetailNames, 'privateMethod')));
assert_isequal(privateMethodDetail.Access, 'private');
sealedMethodDetail = derivedMeta.MethodDetails(find(strcmp(derivedMethodDetailNames, 'sealedMethod')));
assert_istrue(sealedMethodDetail.Sealed);
%=============================================================================
rmpath(path_test);
%=============================================================================
