%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
class_file = [path_test, '/@ClassdefExternalContinuation/ClassdefExternalContinuation.m'];
asserts.isequal(parsefile(class_file), 'classdef');
%=============================================================================
obj = ClassdefExternalContinuation();
[left, right] = obj.splitExternal(7);
asserts.isequal(left, 10);
asserts.isequal(right, 7);
[left, right] = splitExternal(obj, 8);
asserts.isequal(left, 10);
asserts.isequal(right, 8);
asserts.isequal(obj.splitExternal(6), 10);
asserts.isequal(ClassdefExternalContinuation.staticFold(4), 9);
constructorObj = ClassdefContinuationConstructor(41, 42);
asserts.isequal(constructorObj.Value, 41);
asserts.isequal(constructorObj.Other, 42);
%=============================================================================
asserts.istrue(ismethod(obj, 'splitExternal'));
asserts.istrue(any(strcmp(methods('ClassdefExternalContinuation'), 'splitExternal')));
asserts.istrue(any(strcmp(methods('ClassdefExternalContinuation'), 'staticFold')));
meta = metaclass('ClassdefExternalContinuation');
methodNames = {meta.MethodDetails.Name};
splitDetail = meta.MethodDetails(find(strcmp(methodNames, 'splitExternal')));
asserts.isequal(splitDetail.Access, 'public');
asserts.isfalse(splitDetail.Static);
staticDetail = meta.MethodDetails(find(strcmp(methodNames, 'staticFold')));
asserts.istrue(staticDetail.Static);
%=============================================================================
rmpath(path_test);
%=============================================================================
