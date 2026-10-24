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
addpath(path_test, '-begin');
clear functions
%=============================================================================
run([path_test, '/importScriptAfterUseForTest.m']);
asserts.isequal(scriptImportObjectForTest.Value, 87);
asserts.isequal(class(scriptImportObjectForTest), 'nelson.classdefpkg.Options');
clear import
asserts.istrue(isempty(import()));
%=============================================================================
asserts.isequal(importScopedClassForInterpreter(), 25);
asserts.checkerror('Options(1)', sprintf(_('Undefined variable or function: %s'), 'Options'));
asserts.isequal(importAfterFirstUseForInterpreter(), 26);
asserts.istrue(isempty(import()));
%=============================================================================
rmpath(path_test);
%=============================================================================
function value = importScopedClassForInterpreter()
  import nelson.classdefpkg.Options
  obj = Options(25);
  value = obj.Value;
end
%=============================================================================
function value = importAfterFirstUseForInterpreter()
  obj = Options(26);
  import nelson.classdefpkg.Options
  value = obj.Value;
end
%=============================================================================
