%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test, '-begin');
clear functions
%=============================================================================
run([path_test, '/importScriptAfterUseForTest.m']);
asserts.isequal(class(scriptImportObjectForTest), 'nelson.classdefpkg.Options');
asserts.isequal(scriptImportObjectForTest.Value, 87);
clear import
asserts.istrue(isempty(import()));
%=============================================================================
import('nelson.classdefpkg.Options');
optionsObject = Options(41);
asserts.isequal(class(optionsObject), 'nelson.classdefpkg.Options');
asserts.isequal(optionsObject.Value, 41);
%=============================================================================
clear import
import('nelson.classdefpkg.options.DeepClass');
deepObject = DeepClass(5);
asserts.isequal(class(deepObject), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(deepObject.Value, 5);
asserts.isequal(DeepClass.staticValue(7), 42);
%=============================================================================
clear import
import('nelson.classdefpkg.options.DeepClass.staticValue');
asserts.isequal(staticValue(8), 48);
%=============================================================================
clear import
import('nelson.classdefpkg.*');
wildcardObject = Options(12);
partialObject = options.DeepClass(6);
asserts.isequal(class(wildcardObject), 'nelson.classdefpkg.Options');
asserts.isequal(class(partialObject), 'nelson.classdefpkg.options.DeepClass');
asserts.isequal(options.DeepClass.staticValue(9), 54);
%=============================================================================
clear import
import('nelson.classdefpkg.Options');
import('nelson.classdefpkg.Options');
import('nelson.classdefpkg.options.DeepClass.staticValue');
L = import();
asserts.isequal(length(L), 2);
asserts.isequal(L{1}, 'nelson.classdefpkg.Options');
asserts.isequal(L{2}, 'nelson.classdefpkg.options.DeepClass.staticValue');
clear import
asserts.istrue(isempty(import()));
%=============================================================================
asserts.isequal(importFunctionScoped(), 33);
asserts.checkerror('Options(1)', _('Undefined variable or function: Options'));
asserts.isequal(importFunctionAfterUse(), 34);
asserts.istrue(isempty(import()));
%=============================================================================
asserts.checkerror('import(''nelson..Options'')', _('A valid import name expected.'));
asserts.checkerror('import(''nelson.classdefpkg.*.Options'')', _('A valid import name expected.'));
msg = _('Unable to find or import ''nelson.classdefpkg.Missing''. Imported names must end with ''.*'' or be fully qualified.');
asserts.checkerror('import(''nelson.classdefpkg.Missing'')', msg);
%=============================================================================
clear import
rmpath(path_test);
pathTest = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(pathTest, '-begin');
clear import
import('nelson.classdefpkg.Options');
optionsObject = Options(41);
asserts.isequal(class(optionsObject), 'nelson.classdefpkg.Options');
asserts.isequal(optionsObject.Value, 41);
importList = import();
asserts.isequal(importList, {'nelson.classdefpkg.Options'});
clear import
asserts.istrue(isempty(import()));
rmpath(pathTest);
%=============================================================================
function value = importFunctionScoped()
  import nelson.classdefpkg.Options
  obj = Options(33);
  value = obj.Value;
end
%=============================================================================
function value = importFunctionAfterUse()
  obj = Options(34);
  import nelson.classdefpkg.Options
  value = obj.Value;
end
%=============================================================================
