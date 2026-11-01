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
dbclear all
dbstop in for_cli at 12
bp = dbstatus();
asserts.isequal(bp.name, 'for_cli');
dbclear all
asserts.istrue(isempty(dbstatus()));
%=============================================================================
% dbclear in file at line uses the same line adjustment as dbstop
dbstop in for_cli at 12
dbstop in for_cli at 13
dbclear in for_cli at 12
bp = dbstatus();
asserts.isequal(bp.line, 13);
% line 11 is a comment: dbstop would move it to 12, so does dbclear
dbstop in for_cli at 12
dbclear in for_cli at 11
bp = dbstatus();
asserts.isequal(bp.line, 13);
% nothing matches: silent
dbclear in for_cli at 13
asserts.istrue(isempty(dbstatus()));
dbclear in for_cli at 13
dbclear in for_cli
dbclear all in for_cli
asserts.istrue(isempty(dbstatus()));
%=============================================================================
% dbclear in file and dbclear all in file keep the error breakpoints
dbstop in for_cli at 12
dbstop in for_cli at 13
dbstop if error
dbclear in for_cli
bp = dbstatus();
asserts.isequal(size(bp), [1 1]);
asserts.isequal(bp.cond, 'error');
dbstop in for_cli at 12
dbclear all in for_cli
bp = dbstatus();
asserts.isequal(size(bp), [1 1]);
asserts.isequal(bp.cond, 'error');
% dbclear with no argument removes everything, error breakpoints included
dbclear
asserts.istrue(isempty(dbstatus()));
%=============================================================================
asserts.checkerror('dbclear foo', _('Invalid dbclear syntax.'), 'Nelson:dbclear:invalidArgument');
asserts.checkerror('dbclear in for_cli at', _('Invalid dbclear syntax.'), 'Nelson:dbclear:invalidArgument');
dbclear all
%=============================================================================
