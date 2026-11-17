%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('what'), 1);
asserts.isequal(nargout('what'), -1);
%=============================================================================
[builtinList, macroList] = what();
asserts.istrue(iscell(builtinList));
asserts.istrue(iscell(macroList));
asserts.istrue(any(strcmp(builtinList, 'what')));
asserts.istrue(any(strcmp(macroList, 'test_run')));
%=============================================================================
matrix = common_functions_matrix();
asserts.istrue(isstruct(matrix));
asserts.istrue(length(matrix) >= 180);
asserts.isequal(fieldnames(matrix), {'name'; 'area'; 'status'; 'tier'});
[builtinList, macroList] = what();
asserts.istrue(iscell(builtinList));
asserts.istrue(iscell(macroList));
asserts.istrue(any(strcmp(builtinList, 'what')));
asserts.istrue(any(strcmp(macroList, 'test_run')));
%=============================================================================
% module-scoped form: what(moduleName) returns only that module's functions
[globalBuiltin, globalMacro] = what();
[fmBuiltin, fmMacro] = what('functions_manager');
asserts.istrue(iscell(fmBuiltin));
asserts.istrue(iscell(fmMacro));
asserts.istrue(any(strcmp(fmBuiltin, 'what')));
asserts.istrue(numel(fmBuiltin) < numel(globalBuiltin));
asserts.istrue(all(ismember(fmBuiltin, globalBuiltin)));
asserts.istrue(all(ismember(fmMacro, globalMacro)));
%=============================================================================
% the macro belongs to the module that ships it
[~, tmMacro] = what('tests_manager');
asserts.istrue(any(strcmp(tmMacro, 'test_run')));
%=============================================================================
% single output works with the module form too
onlyBuiltin = what('functions_manager');
asserts.istrue(iscell(onlyBuiltin));
asserts.istrue(any(strcmp(onlyBuiltin, 'what')));
%=============================================================================
% an unknown module name is rejected
asserts.checkerror('what(''this_is_not_a_module'')', ...
  _('Unknown module name:') + " 'this_is_not_a_module'.");
%=============================================================================
