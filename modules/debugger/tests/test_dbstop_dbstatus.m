%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([modulepath('debugger', 'tests'), '/cases/']);
%=============================================================================
dbclear all
dbstop in for_cli at 11
dbstop in for_cli at 13
dbstop in for_cli at 14
dbstop in for_cli at 15
dbstop in for_cli at 16
bp = dbstatus();
assert_isequal(bp.name, 'for_cli');
assert_istrue(isfile(bp.file));
assert_isequal(length(bp.line), 4);
assert_isequal(bp.line,[12  13  14  16]);
%=============================================================================
dbclear all
dbstop in function_cli>fun2
bp = dbstatus();
assert_isequal(bp.name, 'function_cli>fun2');
assert_istrue(isfile(bp.file));
assert_isequal(bp.line, 31);
%=============================================================================
dbclear all
dbstop in test_run>getStatusCharacter
bp = dbstatus();
assert_isequal(bp.name, 'test_run>getStatusCharacter');
assert_istrue(isfile(bp.file));
assert_isequal(bp.line, 71);
%=============================================================================