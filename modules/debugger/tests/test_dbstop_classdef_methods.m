%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A class file compiles to one function per method, and the class name resolves
% to the constructor alone. Asking for a line that a method declares, rather
% than one it executes, is what tells the two apart: the answer is that method's
% own first statement, which only the method that owns the line can supply.
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% ClassdefPoint.m line 20 declares magnitude, line 21 is its only statement.
dbclear all
dbstop in ClassdefPoint at 20
bp = dbstatus();
asserts.isequal(bp.name, 'ClassdefPoint');
asserts.istrue(endsWith(bp.file, 'ClassdefPoint.m'));
asserts.isequal(bp.line, 21);
%=============================================================================
% line 24 declares shift, line 25 is its first statement
dbclear all
dbstop in ClassdefPoint at 24
bp = dbstatus();
asserts.isequal(bp.line, 25);
%=============================================================================
% line 31 declares the static method origin, line 32 is its first statement
dbclear all
dbstop in ClassdefPoint at 31
bp = dbstatus();
asserts.isequal(bp.line, 32);
%=============================================================================
% the constructor still answers for its own lines: 13 declares it, 14 opens the
% body, and both are reported as executable
dbclear all
dbstop in ClassdefPoint at 14
bp = dbstatus();
asserts.isequal(bp.line, 14);
%=============================================================================
% line 30 opens the methods (Static) block and line 36 is past the end of the
% file: no method body covers either
dbclear all
asserts.checkerror('dbstop in ClassdefPoint at 30', ...
  _('Cannot set breakpoint at line 30: no executable statement found.'), ...
  'Nelson:dbstop:invalidArgument');
asserts.checkerror('dbstop in ClassdefPoint at 36', ...
  _('Cannot set breakpoint at line 36: no executable statement found.'), ...
  'Nelson:dbstop:invalidArgument');
%=============================================================================
% a handle class exposes delete as the generated destructor protocol; the
% written body, declared line 78 of ClassdefCounter.m, still owns line 79
dbclear all
dbstop in ClassdefCounter at 78
bp = dbstatus();
asserts.isequal(bp.name, 'ClassdefCounter');
asserts.istrue(endsWith(bp.file, 'ClassdefCounter.m'));
asserts.isequal(bp.line, 79);
%=============================================================================
dbclear all
%=============================================================================
