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
asserts.isequal(parsefile([path_test, '/ClassdefAbstractMethodBase.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractMethodMissing.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractMethodImpl.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefPrivateAbstractMethod.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefProtectedAbstractMethod.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefProtectedAbstractMethodImpl.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyBase.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyMissing.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyImpl.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyMixinA.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyMixinB.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyMultiMissing.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyMultiImpl.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyVariants.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyVariantsImpl.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyDefaultValue.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractPropertyDefaultValueImpl.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractSamePropertyA.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractSamePropertyB.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractSamePropertyImpl.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefConcreteSameProperty.m']), 'classdef');
asserts.isequal(parsefile([path_test, '/ClassdefAbstractConcreteSameProperty.m']), 'classdef');
%=============================================================================
baseMeta = metaclass('ClassdefAbstractMethodBase');
asserts.istrue(baseMeta.ClassDetails.Abstract);
baseMethodNames = {baseMeta.MethodDetails.Name};
baseMethod = baseMeta.MethodDetails(find(strcmp(baseMethodNames, 'requiredValue')));
asserts.istrue(baseMethod.Abstract);
asserts.isfalse(baseMethod.Static);
visibleBaseMethods = methods('ClassdefAbstractMethodBase');
asserts.istrue(any(strcmp(visibleBaseMethods, 'requiredValue')));
%=============================================================================
propertyBaseMeta = metaclass('ClassdefAbstractPropertyBase');
asserts.istrue(propertyBaseMeta.ClassDetails.Abstract);
basePropertyNames = {propertyBaseMeta.PropertyDetails.Name};
baseProperty = propertyBaseMeta.PropertyDetails(find(strcmp(basePropertyNames, 'RequiredValue')));
asserts.istrue(baseProperty.Abstract);
asserts.istrue(any(strcmp(baseProperty.Attributes, 'Abstract')));
protectedBaseProperty = propertyBaseMeta.PropertyDetails(find(strcmp(basePropertyNames, 'ProtectedValue')));
asserts.istrue(protectedBaseProperty.Abstract);
asserts.isequal(protectedBaseProperty.GetAccess, 'protected');
asserts.isequal(protectedBaseProperty.ValidationExpression, '(1, 1) double');
%=============================================================================
asserts.checkerror('ClassdefAbstractMethodBase()', ...
  'Cannot instantiate abstract class: ClassdefAbstractMethodBase');
asserts.checkerror('ClassdefAbstractMethodMissing()', ...
  'Cannot instantiate abstract class: ClassdefAbstractMethodMissing');
missingMethodMeta = metaclass('ClassdefAbstractMethodMissing');
asserts.istrue(missingMethodMeta.ClassDetails.Abstract);
privateAbstractMeta = metaclass('ClassdefPrivateAbstractMethod');
asserts.istrue(privateAbstractMeta.ClassDetails.Abstract);
privateMethodNames = {privateAbstractMeta.MethodDetails.Name};
privateMethod = privateAbstractMeta.MethodDetails(find(strcmp(privateMethodNames, 'hiddenValue')));
asserts.isequal(privateMethod.Access, 'private');
asserts.istrue(privateMethod.Abstract);
asserts.checkerror('ClassdefPrivateAbstractMethod()', ...
  'Abstract method cannot have private Access: hiddenValue');
protectedAbstractMeta = metaclass('ClassdefProtectedAbstractMethod');
asserts.istrue(protectedAbstractMeta.ClassDetails.Abstract);
protectedMethodNames = {protectedAbstractMeta.MethodDetails.Name};
protectedMethod = protectedAbstractMeta.MethodDetails(find(strcmp(protectedMethodNames, 'protectedValue')));
asserts.isequal(protectedMethod.Access, 'protected');
asserts.istrue(protectedMethod.Abstract);
visibleProtectedMethods = methods('ClassdefProtectedAbstractMethod');
asserts.isfalse(any(strcmp(visibleProtectedMethods, 'protectedValue')));
asserts.checkerror('ClassdefAbstractPropertyBase()', ...
  'Cannot instantiate abstract class: ClassdefAbstractPropertyBase');
asserts.checkerror('ClassdefAbstractPropertyMissing()', ...
  'Cannot instantiate abstract class: ClassdefAbstractPropertyMissing');
missingPropertyMeta = metaclass('ClassdefAbstractPropertyMissing');
asserts.istrue(missingPropertyMeta.ClassDetails.Abstract);
variantMeta = metaclass('ClassdefAbstractPropertyVariants');
asserts.istrue(variantMeta.ClassDetails.Abstract);
variantPropertyNames = {variantMeta.PropertyDetails.Name};
variantConstant = variantMeta.PropertyDetails(find(strcmp(variantPropertyNames, 'ConstantValue')));
variantDependent = variantMeta.PropertyDetails(find(strcmp(variantPropertyNames, 'DependentValue')));
variantHidden = variantMeta.PropertyDetails(find(strcmp(variantPropertyNames, 'HiddenValue')));
asserts.istrue(variantConstant.Abstract);
asserts.istrue(variantConstant.Constant);
asserts.istrue(variantDependent.Abstract);
asserts.istrue(variantDependent.Dependent);
asserts.istrue(variantHidden.Abstract);
asserts.istrue(variantHidden.Hidden);
visibleVariantProperties = properties('ClassdefAbstractPropertyVariants');
asserts.istrue(any(strcmp(visibleVariantProperties, 'ConstantValue')));
asserts.istrue(any(strcmp(visibleVariantProperties, 'DependentValue')));
asserts.isfalse(any(strcmp(visibleVariantProperties, 'HiddenValue')));
defaultValueMeta = metaclass('ClassdefAbstractPropertyDefaultValue');
asserts.istrue(defaultValueMeta.ClassDetails.Abstract);
defaultValueNames = {defaultValueMeta.PropertyDetails.Name};
defaultValueDetail = defaultValueMeta.PropertyDetails(find(strcmp(defaultValueNames, 'Value')));
asserts.istrue(defaultValueDetail.Abstract);
asserts.isequal(defaultValueDetail.DefaultValueExpression, '1');
asserts.checkerror('ClassdefAbstractPropertyDefaultValue()', ...
  'Abstract property cannot have an initial value: Value');
asserts.checkerror('ClassdefAbstractPropertyDefaultValueImpl()', ...
  'Abstract property cannot have an initial value: Value');
asserts.checkerror('ClassdefAbstractPropertyMultiMissing()', ...
  'Cannot instantiate abstract class: ClassdefAbstractPropertyMultiMissing');
multiMissingMeta = metaclass('ClassdefAbstractPropertyMultiMissing');
asserts.istrue(multiMissingMeta.ClassDetails.Abstract);
%=============================================================================
obj = ClassdefAbstractMethodImpl();
asserts.isequal(obj.requiredValue(), 42);
implMeta = metaclass('ClassdefAbstractMethodImpl');
implMethodNames = {implMeta.MethodDetails.Name};
implMethod = implMeta.MethodDetails(find(strcmp(implMethodNames, 'requiredValue')));
asserts.isfalse(implMethod.Abstract);
asserts.isequal(implMethod.DefiningClass, 'ClassdefAbstractMethodImpl');
protectedMethodObj = ClassdefProtectedAbstractMethodImpl();
asserts.isequal(protectedMethodObj.readValue(), 6);
protectedImplMeta = metaclass('ClassdefProtectedAbstractMethodImpl');
asserts.isfalse(protectedImplMeta.ClassDetails.Abstract);
protectedImplMethodNames = {protectedImplMeta.MethodDetails.Name};
protectedImplMethod = protectedImplMeta.MethodDetails(find(strcmp(protectedImplMethodNames, 'protectedValue')));
asserts.isequal(protectedImplMethod.Access, 'protected');
asserts.isfalse(protectedImplMethod.Abstract);
propertyObj = ClassdefAbstractPropertyImpl();
asserts.isequal(propertyObj.RequiredValue, 43);
asserts.isequal(propertyObj.protectedValue(), 44);
propertyImplMeta = metaclass('ClassdefAbstractPropertyImpl');
propertyImplNames = {propertyImplMeta.PropertyDetails.Name};
implProperty = propertyImplMeta.PropertyDetails(find(strcmp(propertyImplNames, 'RequiredValue')));
asserts.isfalse(implProperty.Abstract);
asserts.isequal(implProperty.DefiningClass, 'ClassdefAbstractPropertyImpl');
implProtectedProperty = propertyImplMeta.PropertyDetails(find(strcmp(propertyImplNames, 'ProtectedValue')));
asserts.isfalse(implProtectedProperty.Abstract);
asserts.isequal(implProtectedProperty.DefiningClass, 'ClassdefAbstractPropertyImpl');
asserts.isequal(implProtectedProperty.GetAccess, 'protected');
asserts.isequal(implProtectedProperty.ValidationExpression, '(1, 1) double');
multiObj = ClassdefAbstractPropertyMultiImpl();
asserts.isequal(multiObj.FirstValue, 1);
asserts.isequal(multiObj.SecondValue, 2);
multiMeta = metaclass('ClassdefAbstractPropertyMultiImpl');
asserts.isfalse(multiMeta.ClassDetails.Abstract);
multiPropertyNames = {multiMeta.PropertyDetails.Name};
multiFirst = multiMeta.PropertyDetails(find(strcmp(multiPropertyNames, 'FirstValue')));
multiSecond = multiMeta.PropertyDetails(find(strcmp(multiPropertyNames, 'SecondValue')));
asserts.isfalse(multiFirst.Abstract);
asserts.isfalse(multiSecond.Abstract);
variantObj = ClassdefAbstractPropertyVariantsImpl();
asserts.isequal(variantObj.ConstantValue, 10);
asserts.isequal(variantObj.DependentValue, 20);
variantImplMeta = metaclass('ClassdefAbstractPropertyVariantsImpl');
asserts.isfalse(variantImplMeta.ClassDetails.Abstract);
variantImplNames = {variantImplMeta.PropertyDetails.Name};
variantImplConstant = variantImplMeta.PropertyDetails(find(strcmp(variantImplNames, 'ConstantValue')));
variantImplDependent = variantImplMeta.PropertyDetails(find(strcmp(variantImplNames, 'DependentValue')));
variantImplHidden = variantImplMeta.PropertyDetails(find(strcmp(variantImplNames, 'HiddenValue')));
asserts.isfalse(variantImplConstant.Abstract);
asserts.istrue(variantImplConstant.Constant);
asserts.isfalse(variantImplDependent.Abstract);
asserts.istrue(variantImplDependent.Dependent);
asserts.isfalse(variantImplHidden.Abstract);
asserts.istrue(variantImplHidden.Hidden);
visibleVariantImplProperties = properties('ClassdefAbstractPropertyVariantsImpl');
asserts.istrue(any(strcmp(visibleVariantImplProperties, 'ConstantValue')));
asserts.istrue(any(strcmp(visibleVariantImplProperties, 'DependentValue')));
asserts.isfalse(any(strcmp(visibleVariantImplProperties, 'HiddenValue')));
visibleProtectedImplMethods = methods('ClassdefProtectedAbstractMethodImpl');
asserts.istrue(any(strcmp(visibleProtectedImplMethods, 'readValue')));
asserts.isfalse(any(strcmp(visibleProtectedImplMethods, 'protectedValue')));
sameAbstractObj = ClassdefAbstractSamePropertyImpl();
asserts.isequal(sameAbstractObj.Shared, 1);
sameAbstractMeta = metaclass('ClassdefAbstractSamePropertyImpl');
asserts.isfalse(sameAbstractMeta.ClassDetails.Abstract);
sameAbstractNames = {sameAbstractMeta.PropertyDetails.Name};
sameAbstractDetail = sameAbstractMeta.PropertyDetails(find(strcmp(sameAbstractNames, 'Shared')));
asserts.isfalse(sameAbstractDetail.Abstract);
asserts.isequal(sameAbstractDetail.DefiningClass, 'ClassdefAbstractSamePropertyImpl');
sameConcreteObj = ClassdefAbstractConcreteSameProperty();
asserts.isequal(sameConcreteObj.Shared, 2);
sameConcreteMeta = metaclass('ClassdefAbstractConcreteSameProperty');
asserts.isfalse(sameConcreteMeta.ClassDetails.Abstract);
sameConcreteNames = {sameConcreteMeta.PropertyDetails.Name};
sameConcreteDetail = sameConcreteMeta.PropertyDetails(find(strcmp(sameConcreteNames, 'Shared')));
asserts.isfalse(sameConcreteDetail.Abstract);
asserts.isequal(sameConcreteDetail.DefiningClass, 'ClassdefConcreteSameProperty');
%=============================================================================
rmpath(path_test);
%=============================================================================
