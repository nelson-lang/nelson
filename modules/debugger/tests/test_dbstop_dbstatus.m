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
dbstop in for_cli at 1
bp = dbstatus();
asserts.isequal(bp.name, 'for_cli');
asserts.isequal(bp.line, 12);
%=============================================================================
dbclear all
dbstop in test_run
bp = dbstatus();
asserts.isequal(bp.name, 'test_run');
asserts.isequal(bp.line, 11);
%=============================================================================
dbclear all
dbstop in for_cli at 12
dbstop in for_cli at 13
dbstop in for_cli at 14
dbstop in for_cli at 15
dbstop in for_cli at 16
bp = dbstatus();
asserts.isequal(bp.name, 'for_cli');
asserts.istrue(isfile(bp.file));
asserts.isequal(length(bp.line), 4);
asserts.isequal(bp.line, [12 13 14 16]);
%=============================================================================
dbclear all
dbstop in function_cli>fun2
bp = dbstatus();
asserts.isequal(bp.name, 'function_cli>fun2');
asserts.istrue(isfile(bp.file));
asserts.isequal(bp.line, 31);
%=============================================================================
dbclear all
dbstop in test_run>getStatusCharacter
bp = dbstatus();
asserts.isequal(bp.name, 'test_run>getStatusCharacter');
asserts.istrue(isfile(bp.file));
asserts.isequal(bp.line, 71);
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
dbclear all
dbstop in ClassdefPoint at 25
bp = dbstatus();
asserts.isequal(bp.name, 'ClassdefPoint');
asserts.istrue(endsWith(bp.file, 'ClassdefPoint.m'));
asserts.isequal(bp.line, 25);
%=============================================================================
dbclear all
dbstop in ClassdefPoint at 32
bp = dbstatus();
asserts.isequal(bp.name, 'ClassdefPoint');
asserts.istrue(endsWith(bp.file, 'ClassdefPoint.m'));
asserts.isequal(bp.line, 32);
%=============================================================================
dbclear all
