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
counterDefault = ClassdefCounter();
asserts.isequal(class(counterDefault), 'ClassdefCounter');
asserts.isequal(counterDefault.Count, 0);
asserts.isequal(counterDefault.LastEvent, '');
asserts.isequal(counterDefault.protectedCount(), 100);
asserts.isequal(counterDefault.writeOnlyCount(), 88);
counterDefault.Count = 12;
asserts.isequal(counterDefault.Count, 12);
failed = false;
try
  __classdef_handle_set_stored__(counterDefault, 'Count', 99);
catch
  failed = true;
end
asserts.istrue(failed);
asserts.isequal(counterDefault.Count, 12);
counterDefault.setStoredCount(21);
asserts.isequal(counterDefault.Count, 21);
failed = false;
try
  counterDefault.setStoredConstant(6);
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
asserts.isequal(parsefile([path_test, '/ClassdefPoint.m']), 'classdef');
%=============================================================================
p = ClassdefPoint(3, 4);
asserts.isequal(class(p), 'ClassdefPoint');
asserts.istrue(isa(p, 'ClassdefPoint'));
asserts.istrue(isobject(p));
asserts.isequal(p.X, 3);
asserts.isequal(p.Y, 4);
asserts.isequal(p.Label, 'point');
asserts.isequal(p.Dimension, 2);
asserts.checkerror('p.Dimension = 3;', 'Cannot modify constant property: Dimension');
pointStruct = struct(p);
asserts.isequal(pointStruct.X, 3);
asserts.isequal(pointStruct.Y, 4);
pointWhos = whos('p');
asserts.isequal(pointWhos.class, 'ClassdefPoint');
pointDisplay = evalc('p');
asserts.istrue(contains(pointDisplay, 'ClassdefPoint'));
asserts.istrue(contains(pointDisplay, 'with properties'));
pointCellDisplay = evalc('pointCell = {p}; pointCell');
asserts.istrue(contains(pointCellDisplay, 'ClassdefPoint'));
repeatedPoint = repmat(p, 1, 2);
asserts.isequal(class(repeatedPoint), 'ClassdefPoint');
asserts.isequal(size(repeatedPoint), [1 2]);
asserts.isequal(repeatedPoint(2).X, 3);
asserts.istrue(any(strcmp(methods(repeatedPoint), 'magnitude')));
try
  double(p);
  error('double should require a classdef overload');
catch M
  asserts.istrue(contains(M.message, 'double'));
end
try
  logical(p);
  error('logical should require a classdef overload');
catch M
  asserts.istrue(contains(M.message, 'logical') && contains(M.message, 'ClassdefPoint'));
end
try
  min(p);
  error('min should require a classdef overload');
catch M
  asserts.istrue(contains(M.message, 'min'));
end
try
  mean(p);
  error('mean should require a classdef overload');
catch M
  asserts.istrue(contains(M.message, 'mean'));
end
try
  abs(p);
  error('abs should require a classdef overload');
catch M
  asserts.istrue(contains(M.message, 'abs'));
end
try
  real(p);
  error('real should reject classdef values without overload');
catch M
  asserts.istrue(contains(M.message, 'real'));
end
try
  imag(p);
  error('imag should reject classdef values without overload');
catch M
  asserts.istrue(contains(M.message, 'imag'));
end
try
  round(p);
  error('round should reject classdef values without overload');
catch M
  asserts.istrue(contains(M.message, 'round'));
end
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'jsonencode');
asserts.checkerror('jsonencode(p);', msg);
persistPoint = ClassdefPoint(6, 8);
persistPointFile = fullfile(fixtureDirectory, 'classdef_point_roundtrip.nh5');
save(persistPointFile, 'persistPoint');
clear persistPoint;
clear classes;
load(persistPointFile);
asserts.isequal(class(persistPoint), 'ClassdefPoint');
asserts.isequal(persistPoint.X, 6);
asserts.isequal(persistPoint.Y, 8);
asserts.isequal(persistPoint.Dimension, 2);
asserts.isequal(persistPoint.magnitude(), 10);
persistPointStruct = struct(persistPoint);
asserts.isequal(fieldnames(persistPointStruct), {'X'; 'Y'; 'Label'});
persistPointDefault = ClassdefPoint(9, 12);
persistPointDefaultBase = fullfile(fixtureDirectory, 'classdef_point_default_roundtrip');
persistPointDefaultFile = [persistPointDefaultBase, '.nh5'];
if isfile(persistPointDefaultFile)
  rmfile(persistPointDefaultFile);
end
save(persistPointDefaultBase, 'persistPointDefault');
clear persistPointDefault;
clear classes;
load(persistPointDefaultBase);
asserts.isequal(class(persistPointDefault), 'ClassdefPoint');
asserts.isequal(persistPointDefault.X, 9);
asserts.isequal(persistPointDefault.Y, 12);
asserts.isequal(persistPointDefault.Dimension, 2);
%=============================================================================
p = ClassdefPoint(3, 4);
asserts.isequal(magnitude(p), 5);
printedMagnitude = evalc('magnitude(p)');
asserts.istrue(contains(printedMagnitude, 'ans ='));
asserts.istrue(contains(printedMagnitude, '5'));
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
      && profInfo(k).LinePosition == 30
    foundSqrtAtMethodLine = true;
  end
end
asserts.istrue(foundSqrtAtMethodLine);
profile('clear');
dbclear all;
dbstop in ClassdefPoint at 30;
bp = dbstatus();
asserts.isequal(bp.name, 'ClassdefPoint');
asserts.istrue(isfile(bp.file));
asserts.isequal(bp.line, 30);
dbclear all;
q = p.shift(1, 2);
asserts.isequal(q.X, 4);
asserts.isequal(q.Y, 6);
asserts.isequal(p.X, 3);
asserts.isequal(p.Y, 4);
%=============================================================================
origin = useClassdefPointStatic();
asserts.isequal(class(origin), 'ClassdefPoint');
asserts.isequal(origin.X, 0);
asserts.isequal(origin.Y, 0);
asserts.isequal(useClassdefPointConstant(), 2);
%=============================================================================
m = methods('ClassdefPoint');
asserts.istrue(any(strcmp(m, 'magnitude')));
asserts.istrue(any(strcmp(m, 'origin')));
asserts.istrue(any(strcmp(m, 'shift')));
props = properties(p);
asserts.istrue(any(strcmp(props, 'X')));
asserts.istrue(any(strcmp(props, 'Y')));
asserts.istrue(any(strcmp(props, 'Label')));
asserts.istrue(any(strcmp(props, 'Dimension')));
asserts.istrue(ismethod(p, 'magnitude'));
asserts.istrue(ismethod('ClassdefPoint', 'origin'));
asserts.isfalse(ismethod(p, 'missingMethod'));
asserts.istrue(isprop(p, 'X'));
asserts.isfalse(isprop(p, 'missingProperty'));
pointArray = [p, q];
asserts.isequal(class(pointArray), 'ClassdefPoint');
asserts.isequal(size(pointArray), [1 2]);
asserts.isequal(pointArray(2).X, 4);
asserts.isequal(pointArray(2).Y, 6);
asserts.isequal(isprop(pointArray, 'X'), [true, true]);
asserts.isequal(isprop(pointArray, 'missingProperty'), [false, false]);
asserts.istrue(any(strcmp(methods(pointArray), 'magnitude')));
asserts.istrue(any(strcmp(properties(pointArray), 'Dimension')));
pointColumn = [p; q];
asserts.isequal(class(pointColumn), 'ClassdefPoint');
asserts.isequal(size(pointColumn), [2 1]);
asserts.isequal(pointColumn(2).X, 4);
asserts.isequal(pointColumn(2).Y, 6);
asserts.istrue(any(strcmp(methods(pointColumn), 'magnitude')));
asserts.istrue(any(strcmp(properties(pointColumn), 'Dimension')));
pointTransposed = pointArray.';
asserts.isequal(class(pointTransposed), 'ClassdefPoint');
asserts.isequal(size(pointTransposed), [2 1]);
asserts.isequal(pointTransposed(2).X, 4);
asserts.isequal(pointTransposed(2).Y, 6);
asserts.istrue(any(strcmp(methods(pointTransposed), 'magnitude')));
asserts.istrue(any(strcmp(properties(pointTransposed), 'Dimension')));
dependentValue = ClassdefDependentValue(7);
dependentWithBuiltinRight = [dependentValue, 2];
asserts.isequal(class(dependentWithBuiltinRight), 'ClassdefDependentValue');
asserts.isequal(size(dependentWithBuiltinRight), [1 2]);
asserts.isequal(dependentWithBuiltinRight(2).Base, 2);
dependentWithBuiltinLeft = [2, dependentValue];
asserts.isequal(class(dependentWithBuiltinLeft), 'ClassdefDependentValue');
asserts.isequal(size(dependentWithBuiltinLeft), [1 2]);
asserts.isequal(dependentWithBuiltinLeft(1).Base, 2);
dependentColumnRight = [dependentValue; 3];
asserts.isequal(class(dependentColumnRight), 'ClassdefDependentValue');
asserts.isequal(size(dependentColumnRight), [2 1]);
asserts.isequal(dependentColumnRight(2).Base, 3);
dependentColumnLeft = [4; dependentValue];
asserts.isequal(class(dependentColumnLeft), 'ClassdefDependentValue');
asserts.isequal(size(dependentColumnLeft), [2 1]);
asserts.isequal(dependentColumnLeft(1).Base, 4);
try
  [ClassdefPoint(1, 2), dependentValue];
  error('ClassdefPoint conversion should fail');
catch M
  asserts.isfalse(contains(M.message, _('Same class type expected.')));
end
inferiorConcatValue = ClassdefInferiorConcatValue(31);
dominantConcatValue = ClassdefDominantConcatValue(32);
dominantConcatArray = [inferiorConcatValue, dominantConcatValue];
asserts.isequal(class(dominantConcatArray), 'ClassdefDominantConcatValue');
asserts.isequal(size(dominantConcatArray), [1 2]);
asserts.isequal(dominantConcatArray(1).Value, 31);
asserts.isequal(dominantConcatArray(2).Value, 32);
pointIndexed = [ClassdefPoint(1, 2), ClassdefPoint(3, 4), ClassdefPoint(5, 6)];
asserts.isequal(pointIndexed(end).X, 5);
pointReordered = pointIndexed([3 1]);
asserts.isequal(class(pointReordered), 'ClassdefPoint');
asserts.isequal(size(pointReordered), [1 2]);
asserts.isequal(pointReordered(1).Y, 6);
asserts.isequal(pointReordered(2).X, 1);
pointIndexed(2) = [];
asserts.isequal(class(pointIndexed), 'ClassdefPoint');
asserts.isequal(size(pointIndexed), [1 2]);
asserts.isequal(pointIndexed(2).X, 5);
pointGrid(2, 3) = ClassdefPoint(21, 22);
asserts.isequal(class(pointGrid), 'ClassdefPoint');
asserts.isequal(size(pointGrid), [2 3]);
asserts.isequal(pointGrid(1, 1).X, 0);
asserts.isequal(pointGrid(1, 2).Y, 0);
asserts.isequal(pointGrid(2, end).X, 21);
pointGridSubset = pointGrid(:, [1 3]);
asserts.isequal(class(pointGridSubset), 'ClassdefPoint');
asserts.isequal(size(pointGridSubset), [2 2]);
asserts.isequal(pointGridSubset(2, 2).Y, 22);
emptyPoints = ClassdefPoint.empty();
asserts.isequal(class(emptyPoints), 'ClassdefPoint');
asserts.isequal(size(emptyPoints), [0 0]);
emptyPointRow = ClassdefPoint.empty(0, 3);
asserts.isequal(class(emptyPointRow), 'ClassdefPoint');
asserts.isequal(size(emptyPointRow), [0 3]);
emptyPointColumn = ClassdefPoint.empty([4 0]);
asserts.isequal(class(emptyPointColumn), 'ClassdefPoint');
asserts.isequal(size(emptyPointColumn), [4 0]);
emptyPointRowConcat = [ClassdefPoint.empty(0, 2), ClassdefPoint.empty(0, 3)];
asserts.isequal(class(emptyPointRowConcat), 'ClassdefPoint');
asserts.isequal(size(emptyPointRowConcat), [0 5]);
emptyPointColumnConcat = [ClassdefPoint.empty(2, 0); ClassdefPoint.empty(3, 0)];
asserts.isequal(class(emptyPointColumnConcat), 'ClassdefPoint');
asserts.isequal(size(emptyPointColumnConcat), [5 0]);
pointFromEmptyRight = [ClassdefPoint.empty(1, 0), ClassdefPoint(12, 13)];
asserts.isequal(class(pointFromEmptyRight), 'ClassdefPoint');
asserts.isequal(size(pointFromEmptyRight), [1 1]);
asserts.isequal(pointFromEmptyRight.X, 12);
pointFromEmptyBottom = [ClassdefPoint.empty(0, 1); ClassdefPoint(14, 15)];
asserts.isequal(class(pointFromEmptyBottom), 'ClassdefPoint');
asserts.isequal(size(pointFromEmptyBottom), [1 1]);
asserts.isequal(pointFromEmptyBottom.Y, 15);
msg = sprintf(_('The following error occurred converting from %s to %s:\n%s'), 'ClassdefDependentValue', 'ClassdefPoint', _('Not enough input arguments.'));
asserts.checkerror('[ClassdefPoint(1, 2), ClassdefDependentValue(3)];', msg, 'Nelson:UnableToConvert');
asserts.checkerror('[ClassdefPoint.empty(1, 0), ClassdefDependentValue(3)];', msg, 'Nelson:UnableToConvert');
emptyPoints = [ClassdefPoint.empty(0, 2), ClassdefDependentValue.empty(0, 3)];
asserts.isequal(class(emptyPoints), 'ClassdefPoint');
asserts.isequal(size(emptyPoints), [0 5]);
asserts.checkerror('ClassdefPoint.empty(1);', _('Invalid dimensions.'));
emptyCounters = ClassdefCounter.empty();
asserts.isequal(class(emptyCounters), 'ClassdefCounter');
asserts.isequal(size(emptyCounters), [0 0]);
emptyCounterRow = ClassdefCounter.empty(0, 2);
asserts.isequal(class(emptyCounterRow), 'ClassdefCounter');
asserts.isequal(size(emptyCounterRow), [0 2]);
asserts.isequal(size(isvalid(emptyCounterRow)), [0 2]);
emptyCounterRowConcat = [ClassdefCounter.empty(0, 2), ClassdefCounter.empty(0, 3)];
asserts.isequal(class(emptyCounterRowConcat), 'ClassdefCounter');
asserts.isequal(size(emptyCounterRowConcat), [0 5]);
asserts.isequal(size(isvalid(emptyCounterRowConcat)), [0 5]);
emptyCounterColumnConcat = [ClassdefCounter.empty(2, 0); ClassdefCounter.empty(3, 0)];
asserts.isequal(class(emptyCounterColumnConcat), 'ClassdefCounter');
asserts.isequal(size(emptyCounterColumnConcat), [5 0]);
asserts.isequal(size(isvalid(emptyCounterColumnConcat)), [5 0]);
asserts.istrue(any(strcmp(methods(emptyCounterRow), 'increment')));
asserts.istrue(any(strcmp(properties(emptyCounterRow), 'Count')));
counterFromEmptyRight = [ClassdefCounter.empty(1, 0), ClassdefCounter(91)];
asserts.isequal(class(counterFromEmptyRight), 'ClassdefCounter');
asserts.isequal(size(counterFromEmptyRight), [1 1]);
asserts.isequal(counterFromEmptyRight.Count, 91);
delete(counterFromEmptyRight);
counterFromEmptyBottom = [ClassdefCounter.empty(0, 1); ClassdefCounter(92)];
asserts.isequal(class(counterFromEmptyBottom), 'ClassdefCounter');
asserts.isequal(size(counterFromEmptyBottom), [1 1]);
asserts.isequal(counterFromEmptyBottom.Count, 92);
delete(counterFromEmptyBottom);
asserts.checkerror('[ClassdefCounter(1), ClassdefDependentHandle(2)];', _('Handles being catenated have incompatible classes.'));
asserts.checkerror('[ClassdefCounter.empty(1, 0), ClassdefDependentHandle(2)];', _('Handles being catenated have incompatible classes.'));
asserts.checkerror('[ClassdefCounter.empty(0, 2), ClassdefDependentHandle.empty(0, 3)];', _('Handles being catenated have incompatible classes.'));
asserts.checkerror('ClassdefCounter.empty(1);', _('Invalid dimensions.'));
asserts.isequal(fieldnames(p), props);
pointEvents = events(p);
asserts.istrue(isempty(pointEvents));
asserts.isequal(events('ClassdefPoint'), pointEvents);
asserts.checkerror('ClassdefValueEvents()', ...
  'Class may not define events because only handle subclasses may define events.');
asserts.isequal(parsestring('m = ?ClassdefPoint;'), 'script');
pointMeta = metaclass('ClassdefPoint');
pointMetaFromQuery = ?ClassdefPoint;
asserts.isequal(pointMetaFromQuery.Name, pointMeta.Name);
asserts.isequal(class(pointMeta), 'nelson.metadata.Class');
asserts.istrue(isprop(pointMeta, 'PropertyDetails'));
asserts.istrue(isprop(pointMeta, 'MethodDetails'));
asserts.istrue(isprop(pointMeta, 'EventDetails'));
asserts.istrue(isprop(pointMeta, 'EnumerationDetails'));
asserts.istrue(isprop(pointMeta, 'ClassDetails'));
asserts.isequal(pointMeta.ClassDetails.Name, 'ClassdefPoint');
asserts.isfalse(pointMeta.ClassDetails.Abstract);
asserts.isfalse(pointMeta.ClassDetails.Sealed);
asserts.isfalse(pointMeta.ClassDetails.Handle);
pointPropertyDetailNames = {pointMeta.PropertyDetails.Name};
asserts.istrue(any(strcmp(pointPropertyDetailNames, 'Dimension')));
dimensionDetail = pointMeta.PropertyDetails(find(strcmp(pointPropertyDetailNames, 'Dimension')));
asserts.istrue(dimensionDetail.Constant);
asserts.isfalse(dimensionDetail.Abstract);
asserts.isfalse(dimensionDetail.Dynamic);
asserts.isequal(dimensionDetail.GetAccess, 'public');
asserts.isequal(dimensionDetail.SetAccess, 'public');
asserts.istrue(any(strcmp(dimensionDetail.Attributes, 'Constant')));
pointMethodDetailNames = {pointMeta.MethodDetails.Name};
asserts.istrue(any(strcmp(pointMethodDetailNames, 'origin')));
originDetail = pointMeta.MethodDetails(find(strcmp(pointMethodDetailNames, 'origin')));
asserts.istrue(originDetail.Static);
asserts.isfalse(originDetail.Abstract);
asserts.isequal(originDetail.Access, 'public');
asserts.istrue(any(strcmp(originDetail.Attributes, 'Static')));
pointEventDetailNames = {pointMeta.EventDetails.Name};
asserts.istrue(isempty(pointEventDetailNames));
asserts.checkerror('missingMeta = ?ClassdefMissing;', 'Class not found: ClassdefMissing');
%=============================================================================
d = ClassdefDerived(5);
asserts.isequal(class(d), 'ClassdefDerived');
asserts.istrue(isa(d, 'ClassdefDerived'));
asserts.istrue(isa(d, 'ClassdefBase'));
asserts.isfalse(isa(d, 'ClassdefPoint'));
asserts.isequal(d.BaseValue, 10);
asserts.isequal(d.BaseConstant, 7);
asserts.isequal(d.DerivedValue, 5);
asserts.isequal(baseMethod(d), 10);
asserts.isequal(d.baseMethod(), 10);
asserts.isequal(d.derivedMethod(), 15);
asserts.isequal(sealedMethod(d), 'sealed');
asserts.isequal(d.sealedMethod(), 'sealed');
asserts.isequal(d.hiddenMethod(), 99);
asserts.isequal(d.callPrivate(), -2);
asserts.isequal(d.callProtected(), 42);
asserts.isequal(d.cannotCallBasePrivate(), 'blocked');
asserts.isequal(d.readPrivateProperty(), 31);
d = d.setPrivateProperty(32);
asserts.isequal(d.readPrivateProperty(), 32);
asserts.isequal(d.readBaseProtectedProperty(), 41);
asserts.isequal(d.cannotReadBasePrivateProperty(), 'blocked');
asserts.isequal(d.BaseReadOnlyByAccess, 51);
d = d.setReadOnlyProperty(52);
asserts.isequal(d.BaseReadOnlyByAccess, 52);
asserts.checkerror('d.BaseReadOnlyByAccess = 53;', 'Cannot set property: BaseReadOnlyByAccess');
d.BaseWriteOnlyPublic = 62;
asserts.isequal(d.readWriteOnlyProperty(), 62);
asserts.checkerror('d.BaseWriteOnlyPublic;', 'Cannot get property: BaseWriteOnlyPublic');
asserts.isequal(useClassdefDerivedInheritedStatic(), 'base');
asserts.isequal(useClassdefDerivedInheritedConstant(), 7);
asserts.isequal(useClassdefBasePrivateStatic(), 'privateStatic');
asserts.isequal(useClassdefDerivedProtectedStatic(), 'protectedStatic');
asserts.isequal(useClassdefDerivedPrivateStaticBlocked(), 'blocked');
derivedMethods = methods('ClassdefDerived');
asserts.istrue(any(strcmp(derivedMethods, 'baseMethod')));
asserts.istrue(any(strcmp(derivedMethods, 'baseStatic')));
asserts.istrue(any(strcmp(derivedMethods, 'derivedMethod')));
asserts.istrue(any(strcmp(derivedMethods, 'callPrivate')));
asserts.istrue(any(strcmp(derivedMethods, 'callProtected')));
asserts.istrue(any(strcmp(derivedMethods, 'readPrivateProperty')));
asserts.istrue(any(strcmp(derivedMethods, 'readBaseProtectedProperty')));
asserts.istrue(any(strcmp(derivedMethods, 'sealedMethod')));
asserts.isfalse(any(strcmp(derivedMethods, 'hiddenMethod')));
asserts.isfalse(any(strcmp(derivedMethods, 'privateMethod')));
asserts.isfalse(any(strcmp(derivedMethods, 'protectedMethod')));
asserts.isfalse(ismethod(d, 'hiddenMethod'));
asserts.isfalse(ismethod(d, 'privateMethod'));
asserts.isfalse(ismethod(d, 'protectedMethod'));
try
  d.privateMethod();
  error('private method call should fail');
catch M
  asserts.istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, 'Undefined'));
end
try
  privateMethod(d);
  error('private method call should fail');
catch M
  asserts.istrue(contains(M.message, 'Function not found') ...
    || contains(M.message, 'No such property') ...
    || contains(M.message, 'Undefined variable or function') ...
    || contains(M.message, 'undefined.'));
end
try
  d.protectedMethod();
  error('protected method call should fail');
catch M
  asserts.istrue(contains(M.message, 'No such property') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, 'Undefined'));
end
try
  protectedMethod(d);
  error('protected method call should fail');
catch M
  asserts.istrue(contains(M.message, 'Function not found') ...
    || contains(M.message, 'No such property') ...
    || contains(M.message, 'Undefined variable or function') ...
    || contains(M.message, 'undefined.'));
end
asserts.checkerror('d.BasePrivateValue;', 'Cannot get property: BasePrivateValue');
asserts.checkerror('d.BasePrivateValue = 33;', 'Cannot set property: BasePrivateValue');
asserts.checkerror('d.BaseProtectedValue;', 'Cannot get property: BaseProtectedValue');
asserts.checkerror('d.BaseProtectedValue = 42;', 'Cannot set property: BaseProtectedValue');
try
  useClassdefBasePrivateStaticExternal();
  error('private static method call should fail');
catch M
  asserts.istrue(contains(M.message, 'Undefined variable or function') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, 'Cannot get property') ...
    || contains(M.message, 'No such property'));
end
try
  useClassdefBaseProtectedStaticExternal();
  error('protected static method call should fail');
catch M
  asserts.istrue(contains(M.message, 'Undefined variable or function') ...
    || contains(M.message, 'Function not found') ...
    || contains(M.message, 'Reference to non-existent field') ...
    || contains(M.message, 'Cannot get property') ...
    || contains(M.message, 'No such property'));
end
derivedProps = properties(d);
asserts.istrue(any(strcmp(derivedProps, 'BaseValue')));
asserts.istrue(any(strcmp(derivedProps, 'BaseConstant')));
asserts.istrue(any(strcmp(derivedProps, 'BaseReadOnlyByAccess')));
asserts.istrue(any(strcmp(derivedProps, 'DerivedValue')));
asserts.isfalse(any(strcmp(derivedProps, 'BasePrivateValue')));
asserts.isfalse(any(strcmp(derivedProps, 'BaseProtectedValue')));
asserts.isfalse(any(strcmp(derivedProps, 'BaseWriteOnlyPublic')));
derivedEvents = events(d);
asserts.istrue(isempty(derivedEvents));
asserts.checkerror('d.BaseConstant = 8;', 'Cannot modify constant property: BaseConstant');
asserts.checkerror('ClassdefAbstract()', 'Cannot instantiate abstract class: ClassdefAbstract');
asserts.checkerror('ClassdefSealedOverride()', 'Cannot override sealed method: sealedMethod');
asserts.checkerror('ClassdefSealedSubclass()', 'Cannot inherit from sealed class: ClassdefSealedClass');
abstractMeta = metaclass('ClassdefAbstract');
asserts.istrue(abstractMeta.ClassDetails.Abstract);
asserts.istrue(any(strcmp(abstractMeta.ClassDetails.Attributes, 'Abstract')));
sealedClassMeta = metaclass('ClassdefSealedClass');
asserts.istrue(sealedClassMeta.ClassDetails.Sealed);
asserts.istrue(any(strcmp(sealedClassMeta.ClassDetails.Attributes, 'Sealed')));
%=============================================================================
dispatchBase = ClassdefDispatchBase(8);
dispatchChild = ClassdefDispatchChild(9, 10);
asserts.istrue(isa(dispatchChild, 'ClassdefDispatchBase'));
asserts.isequal(dispatchBase.identify(), 'base:8');
asserts.isequal(dispatchChild.identify(), 'child:9:10');
asserts.isequal(identify(dispatchChild), 'child:9:10');
asserts.isequal(dispatchChild.callIdentifyDot(), 'child:9:10');
asserts.isequal(dispatchChild.callIdentifyFunction(), 'child:9:10');
asserts.isequal(dispatchChild.inheritedOnly(), 'inherited:9');
asserts.isequal(ClassdefDispatchBase.staticName(), 'base-static');
asserts.isequal(ClassdefDispatchChild.staticName(), 'child-static');
asserts.isequal(ClassdefDispatchChild.staticOnly(), 'base-only');
dispatchMethods = methods('ClassdefDispatchChild');
asserts.istrue(any(strcmp(dispatchMethods, 'identify')));
asserts.istrue(any(strcmp(dispatchMethods, 'callIdentifyDot')));
asserts.istrue(any(strcmp(dispatchMethods, 'inheritedOnly')));
dispatchMeta = metaclass('ClassdefDispatchChild');
asserts.istrue(any(strcmp({dispatchMeta.SuperclassList.Name}, 'ClassdefDispatchBase')));
dispatchMethodNames = {dispatchMeta.MethodDetails.Name};
dispatchIdentifyDetail = dispatchMeta.MethodDetails(find(strcmp(dispatchMethodNames, 'identify')));
asserts.isequal(dispatchIdentifyDetail.DefiningClass, 'ClassdefDispatchChild');
%=============================================================================
splitObj = ClassdefSplit();
asserts.isequal(class(splitObj), 'ClassdefSplit');
asserts.isequal(externalValue(splitObj), 12);
asserts.isequal(splitObj.externalValue(), 12);
asserts.isequal(splitObj.callPrivateExternal(), 8);
asserts.isequal(useClassdefSplitStaticExternal(), 'splitStatic');
asserts.isequal(useClassdefSplitPrivateExternal(), 'blocked');
splitDerived = ClassdefSplitDerived();
asserts.istrue(isa(splitDerived, 'ClassdefSplit'));
asserts.isequal(splitDerived.callProtectedExternal(), 16);
asserts.checkerror('splitObj.privateExternal();', 'No such property: privateExternal');
asserts.checkerror('splitObj.protectedExternal();', 'No such property: protectedExternal');
asserts.istrue(ismethod(splitObj, 'externalValue'));
asserts.isfalse(ismethod(splitObj, 'privateExternal'));
asserts.isfalse(ismethod(splitObj, 'protectedExternal'));
splitMethods = methods('ClassdefSplit');
asserts.istrue(any(strcmp(splitMethods, 'externalValue')));
asserts.istrue(any(strcmp(splitMethods, 'callPrivateExternal')));
asserts.istrue(any(strcmp(splitMethods, 'staticExternal')));
asserts.isfalse(any(strcmp(splitMethods, 'privateExternal')));
asserts.isfalse(any(strcmp(splitMethods, 'protectedExternal')));
splitMeta = metaclass('ClassdefSplit');
splitMethodDetailNames = {splitMeta.MethodDetails.Name};
staticExternalDetail = splitMeta.MethodDetails(find(strcmp(splitMethodDetailNames, 'staticExternal')));
asserts.istrue(staticExternalDetail.Static);
privateExternalDetail = splitMeta.MethodDetails(find(strcmp(splitMethodDetailNames, 'privateExternal')));
asserts.isequal(privateExternalDetail.Access, 'private');
protectedExternalDetail = splitMeta.MethodDetails(find(strcmp(splitMethodDetailNames, 'protectedExternal')));
asserts.isequal(protectedExternalDetail.Access, 'protected');
%=============================================================================
packaged = useClassdefPackagedConstructor(6);
asserts.isequal(class(packaged), 'classdefpkg.ClassdefPackaged');
asserts.istrue(isa(packaged, 'classdefpkg.ClassdefPackaged'));
asserts.isequal(packaged.Value, 6);
asserts.isequal(packaged.triple(), 18);
asserts.isequal(useClassdefPackagedConstant(), 12);
packagedOrigin = useClassdefPackagedStatic();
asserts.isequal(class(packagedOrigin), 'classdefpkg.ClassdefPackaged');
asserts.isequal(packagedOrigin.Value, 0);
packagedMethods = methods('classdefpkg.ClassdefPackaged');
asserts.istrue(any(strcmp(packagedMethods, 'triple')));
asserts.istrue(any(strcmp(packagedMethods, 'origin')));
packagedProps = properties(packaged);
asserts.istrue(any(strcmp(packagedProps, 'Value')));
asserts.istrue(any(strcmp(packagedProps, 'ConstantValue')));
packagedMeta = metaclass('classdefpkg.ClassdefPackaged');
asserts.isequal(packagedMeta.Name, 'classdefpkg.ClassdefPackaged');
packagedMetaFromQuery = ?classdefpkg.ClassdefPackaged;
asserts.isequal(packagedMetaFromQuery.Name, 'classdefpkg.ClassdefPackaged');
rmpath(path_test);
rmdir(fixtureDirectory, 's');
%=============================================================================
