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
% a Constant property default built from a local function resolves it
asserts.isequal(ClassdefConstantLocalFunction.FromLocal, 99);
%=============================================================================
% a local function that itself calls another local function resolves too
asserts.isequal(ClassdefConstantLocalFunction.Chained, 100);
%=============================================================================
% a literal Constant keeps working
asserts.isequal(ClassdefConstantLocalFunction.Literal, 5);
%=============================================================================
% a self-referencing Constant keeps working
asserts.isequal(ClassdefConstantLocalFunction.Derived, 10);
%=============================================================================
% a local function called from a method keeps working
obj = ClassdefConstantLocalFunction();
asserts.isequal(obj.fromMethod(), 99);
%=============================================================================
% instance access to the Constant resolves the same value
asserts.isequal(obj.FromLocal, 99);
%=============================================================================
rmpath(path_test);
%=============================================================================
