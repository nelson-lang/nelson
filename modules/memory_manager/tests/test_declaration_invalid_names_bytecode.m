%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CLI MODE-->
%=============================================================================
addpath([nelsonroot(), '/modules/memory_manager/tests/']);
%=============================================================================
msg = sprintf(_('Lexical error ''%s'''), _('Argument must contain a valid variable name.'));
asserts.checkerror('global 1', msg);
asserts.checkerror('global ''bytecodeQuotedGlobal''', msg);
asserts.checkerror('global "bytecodeQuotedGlobalString"', msg);
asserts.checkerror('persistent 1', msg);
asserts.checkerror('persistent ''bytecodeQuotedPersistent''', msg);
asserts.checkerror('persistent "bytecodeQuotedPersistentString"', msg);
asserts.isequal(parsestring('global bytecodeGlobalValidName1'), 'script');
global bytecodeGlobalValidName1
clear global bytecodeGlobalValidName1
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
%=============================================================================
filewrite([path_test, '/bytecode_persistent_invalid_numeric_runtime.m'], ...
  ["function r = bytecode_persistent_invalid_numeric_runtime()"; ...
   "  persistent 1"; ...
   "  r = 1;"; ...
   "end"]);
filewrite([path_test, '/bytecode_global_invalid_quoted_runtime.m'], ...
  ["function r = bytecode_global_invalid_quoted_runtime()"; ...
   "  global 'bytecodeQuotedGlobalInFunction'"; ...
   "  r = 1;"; ...
   "end"]);
filewrite([path_test, '/bytecode_persistent_invalid_quoted_runtime.m'], ...
  ["function r = bytecode_persistent_invalid_quoted_runtime()"; ...
   "  persistent 'bytecodeQuotedPersistent'"; ...
   "  r = 1;"; ...
   "end"]);
filewrite([path_test, '/bytecode_global_invalid_double_quoted_runtime.m'], ...
  ["function r = bytecode_global_invalid_double_quoted_runtime()"; ...
   "  global ""bytecodeQuotedGlobalStringInFunction"""; ...
   "  r = 1;"; ...
   "end"]);
filewrite([path_test, '/bytecode_persistent_invalid_double_quoted_runtime.m'], ...
  ["function r = bytecode_persistent_invalid_double_quoted_runtime()"; ...
   "  persistent ""bytecodeQuotedPersistentString"""; ...
   "  r = 1;"; ...
   "end"]);
filewrite([path_test, '/bytecode_declaration_valid_alnum_runtime.m'], ...
  ["function r = bytecode_declaration_valid_alnum_runtime()"; ...
   "  persistent bytecodePersistentValidName1"; ...
   "  global bytecodeGlobalValidName2"; ...
   "  if isempty(bytecodePersistentValidName1)"; ...
   "    bytecodePersistentValidName1 = 10;"; ...
   "  end"; ...
   "  bytecodeGlobalValidName2 = bytecodePersistentValidName1 + 1;"; ...
   "  r = bytecodeGlobalValidName2;"; ...
   "end"]);
addpath(path_test);
%=============================================================================
M = [];
try
  bytecode_persistent_invalid_numeric_runtime();
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, _('Argument must contain a valid variable name.')));
%=============================================================================
M = [];
try
  bytecode_global_invalid_quoted_runtime();
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, _('Argument must contain a valid variable name.')));
%=============================================================================
M = [];
try
  bytecode_persistent_invalid_quoted_runtime();
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, _('Argument must contain a valid variable name.')));
%=============================================================================
M = [];
try
  bytecode_global_invalid_double_quoted_runtime();
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, _('Argument must contain a valid variable name.')));
%=============================================================================
M = [];
try
  bytecode_persistent_invalid_double_quoted_runtime();
catch
  M = lasterror();
end
asserts.istrue(contains(M.message, _('Argument must contain a valid variable name.')));
%=============================================================================
clear bytecode_declaration_valid_alnum_runtime
clear global bytecodeGlobalValidName2
asserts.isequal(bytecode_declaration_valid_alnum_runtime(), 11);
global bytecodeGlobalValidName2
asserts.isequal(bytecodeGlobalValidName2, 11);
clear bytecode_declaration_valid_alnum_runtime
clear global bytecodeGlobalValidName2
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================
