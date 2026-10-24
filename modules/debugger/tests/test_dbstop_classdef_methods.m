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
% ClassdefPoint.m line 29 declares magnitude, line 30 is its only statement.
dbclear all
dbstop in ClassdefPoint at 29
bp = dbstatus();
asserts.isequal(bp.name, 'ClassdefPoint');
asserts.istrue(endsWith(bp.file, 'ClassdefPoint.m'));
asserts.isequal(bp.line, 30);
%=============================================================================
% line 33 declares shift, line 34 is its first statement
dbclear all
dbstop in ClassdefPoint at 33
bp = dbstatus();
asserts.isequal(bp.line, 34);
%=============================================================================
% line 40 declares the static method origin, line 41 is its first statement
dbclear all
dbstop in ClassdefPoint at 40
bp = dbstatus();
asserts.isequal(bp.line, 41);
%=============================================================================
% the constructor still answers for its own lines: 22 declares it, 23 opens the
% body, and both are reported as executable
dbclear all
dbstop in ClassdefPoint at 23
bp = dbstatus();
asserts.isequal(bp.line, 23);
%=============================================================================
% line 39 opens the methods (Static) block and line 45 is past the end of the
% file: no method body covers either
dbclear all
asserts.checkerror('dbstop in ClassdefPoint at 39', ...
  _('Cannot set breakpoint at line 39: no executable statement found.'), ...
  'Nelson:dbstop:invalidArgument');
asserts.checkerror('dbstop in ClassdefPoint at 45', ...
  _('Cannot set breakpoint at line 45: no executable statement found.'), ...
  'Nelson:dbstop:invalidArgument');
%=============================================================================
% a handle class exposes delete as the generated destructor protocol; the
% written body, declared line 87 of ClassdefCounter.m, still owns line 88
dbclear all
dbstop in ClassdefCounter at 87
bp = dbstatus();
asserts.isequal(bp.name, 'ClassdefCounter');
asserts.istrue(endsWith(bp.file, 'ClassdefCounter.m'));
asserts.isequal(bp.line, 88);
%=============================================================================
dbclear all
%=============================================================================
