%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
prefixOptions = nelson.classdefpkg.Options(41);
asserts.isequal(class(prefixOptions), 'nelson.classdefpkg.Options');
asserts.isequal(prefixOptions.Value, 41);
%=============================================================================
deepPackaged = nelson.classdefpkg.options.DeepClass(7);
asserts.isequal(class(deepPackaged), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(deepPackaged.scale(), 35);
deepPackagedFromFunction = makeDeepPackagedClassdefForTest(8);
asserts.isequal(class(deepPackagedFromFunction), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(deepPackagedFromFunction.scale(), 40);
deepPackagedStaticValue = makeDeepPackagedClassdefStaticValueForTest(8);
asserts.isequal(deepPackagedStaticValue, 48);
closureValue = classdefClosureValueForTest(deepPackaged);
asserts.isequal(closureValue, 4);
closureStructValue = classdefStructClosureValueForTest(deepPackaged);
asserts.isequal(closureStructValue, 4);
deepPackagedContainer = nelson.classdefpkg.options.DeepClass(0, deepPackaged);
closureChildValue = classdefChildClosureValueForTest(deepPackagedContainer);
asserts.isequal(closureChildValue, 4);
closureStructChildValue = classdefStructChildClosureValueForTest(deepPackagedContainer);
asserts.isequal(closureStructChildValue, 4);
deepPackagedNameValueContainer = nelson.classdefpkg.options.DeepClass(0, [], 'Child', deepPackaged);
asserts.isequal(class(deepPackagedNameValueContainer.Child), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(deepPackagedNameValueContainer.Child.Value, 7);
deepPackagedWithCallback = nelson.classdefpkg.options.DeepClass(7, [], 'Callback', @(x) x + 1);
deepPackagedCallbackContainer = nelson.classdefpkg.options.DeepClass(0, [], 'Child', deepPackagedWithCallback);
asserts.isequal(class(deepPackagedCallbackContainer.Child), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(deepPackagedCallbackContainer.Child.callCallback(3), 4);
deepPackagedWithCapture = nelson.classdefpkg.options.DeepClass(7, [], ...
  'Callback', @(x) deepPackaged.evaluateWithVector(x));
deepPackagedCaptureContainer = nelson.classdefpkg.options.DeepClass(0, [], 'Child', deepPackagedWithCapture);
asserts.isequal(class(deepPackagedCaptureContainer.Child), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(deepPackagedCaptureContainer.Child.callCallback(5), 4);
deepPackagedObjectiveContainer = nelson.classdefpkg.options.DeepClass(0, [], 'Objective', deepPackagedWithCapture);
asserts.isequal(class(deepPackagedObjectiveContainer.Objective), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(deepPackagedObjectiveContainer.Objective.callCallback(5), 4);
deepPackagedObjectiveWrapper = makeDeepPackagedObjectiveContainerForTest(deepPackagedWithCapture);
asserts.isequal(class(deepPackagedObjectiveWrapper.Objective), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(deepPackagedObjectiveWrapper.Objective.callCallback(5), 4);
asserts.isequal(class(deepPackagedObjectiveWrapper), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(class(deepPackagedObjectiveWrapper.Objective), 'nelson.classdefpkg.options.DeepClass');
asserts.istrue(isa(deepPackagedObjectiveWrapper, 'nelson.classdefpkg.options.DeepClass'));
asserts.isequal(class(deepPackagedObjectiveWrapper.Objective), 'nelson.classdefpkg.options.DeepClass');
deepPackagedObjectiveVararginWrapper = makeDeepPackagedObjectiveContainerVararginForTest(...
  0, [], 'Objective', deepPackagedWithCapture);
asserts.isequal(class(deepPackagedObjectiveVararginWrapper.Objective), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(deepPackagedObjectiveVararginWrapper.Objective.callCallback(5), 4);
optionsObjectiveVararginWrapper = makeDeepPackagedObjectiveContainerVararginForTest(...
  0, [], 'Objective', prefixOptions);
asserts.isequal(class(optionsObjectiveVararginWrapper.Objective), 'nelson.classdefpkg.Options');
asserts.isequal(optionsObjectiveVararginWrapper.Objective.Value, 41);
optionsObjectiveFileWrapper = classdefPackageVararginWrapper(0, [], 'Objective', prefixOptions);
asserts.isequal(class(optionsObjectiveFileWrapper.Objective), 'nelson.classdefpkg.Options');
asserts.isequal(optionsObjectiveFileWrapper.Objective.Value, 41);
optionsObjectiveShortNameWrapper = nelsonclass(0, [], 'Objective', prefixOptions);
asserts.isequal(class(optionsObjectiveShortNameWrapper.Objective), 'nelson.classdefpkg.Options');
asserts.isequal(optionsObjectiveShortNameWrapper.Objective.Value, 41);
%=============================================================================
derivedOptions = nelson.classdefpkg.options.DerivedOptions(9);
asserts.isequal(class(derivedOptions), 'nelson.classdefpkg.options.DerivedOptions');
asserts.istrue(isa(derivedOptions, 'nelson.classdefpkg.Options'));
asserts.istrue(derivedOptions.SetterIsaValue);
asserts.istrue(derivedOptions.LocalIsaValue);
asserts.istrue(derivedOptions.methodLocalIsa());
%=============================================================================
function obj = makeDeepPackagedClassdefForTest(value)
  obj = nelson.classdefpkg.options.DeepClass(value);
end
%=============================================================================
function value = makeDeepPackagedClassdefStaticValueForTest(inputValue)
  value = nelson.classdefpkg.options.DeepClass.staticValue(inputValue);
end
%=============================================================================
function r = classdefClosureValueForTest(obj)
  f = @(x) obj.evaluateWithVector(x);
  r = f(5);
end
%=============================================================================
function r = classdefStructClosureValueForTest(obj)
  s = struct();
  s.f = @(x) obj.evaluateWithVector(x);
  r = feval(s.f, 5);
end
%=============================================================================
function r = classdefChildClosureValueForTest(obj)
  f = @(x) obj.evaluateChildWithVector(x);
  r = f(5);
end
%=============================================================================
function r = classdefStructChildClosureValueForTest(obj)
  s = struct();
  s.f = @(x) obj.evaluateChildWithVector(x);
  r = feval(s.f, 5);
end
%=============================================================================
function obj = makeDeepPackagedObjectiveContainerForTest(child)
  obj = nelson.classdefpkg.options.DeepClass(0, [], 'Objective', child);
end
%=============================================================================
function obj = makeDeepPackagedObjectiveContainerVararginForTest(varargin)
  obj = nelson.classdefpkg.options.DeepClass(varargin{:});
end
%=============================================================================
