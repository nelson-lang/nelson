%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% dbstop / dbclear / dbstatus syntax errors and the local function location form.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NLS0004>
filewrite(fullfile(fixtureDirectory, 'bpl.m'), ['function y = bpl(x)', LF, ...
  'y = 0;', LF, ...
  'y = loc(y);', LF, ...
  'end', LF, ...
  'function z = loc(a)', LF, ...
  '% local', LF, ...
  'z = a + 1;', LF, ...
  'end', LF]);
addpath(fixtureDirectory);
pathCleanup = onCleanup(@() rmpath(fixtureDirectory)); %#ok<NLS0004>
dbclear all
%=============================================================================
% location given as a local function name
dbstop in bpl at loc
b = dbstatus();
asserts.isequal(b.name, 'bpl>loc');
asserts.isequal(b.line, 7);
dbclear all
dbstop in bpl>loc
b = dbstatus();
asserts.isequal(b.name, 'bpl>loc');
asserts.isequal(b.line, 7);
dbclear all
asserts.checkerror('dbstop in bpl>loc at 7', ...
  _('Invalid option combination. Specify the line number or specify the local or nested function.'), ...
  'Nelson:dbstop:invalidOptionCombination');
%=============================================================================
asserts.checkerror('dbstop if foo', _('A file name must be specified.'), 'Nelson:dbstop:fileNameRequired');
asserts.checkerror('dbstop in bpl at 2 if', _('Unknown command option.'), 'Nelson:dbstop:unknownCommandOption');
asserts.checkerror('dbstop foo bar', _('Unknown command option.'), 'Nelson:dbstop:unknownCommandOption');
asserts.checkerror('dbstop in nosuchfn_xyz', _('Cannot find function "nosuchfn_xyz".'), ...
  'Nelson:dbstop:invalidArgument');
asserts.istrue(isempty(dbstatus()));
%=============================================================================
% debug commands outside a debug session
msg = _('Debug commands only allowed when stopped in debug mode.');
asserts.checkerror('dbstep', msg, 'Nelson:debugger:notInDebugMode');
asserts.checkerror('dbstep in', msg, 'Nelson:debugger:notInDebugMode');
asserts.checkerror('dbcont', msg, 'Nelson:debugger:notInDebugMode');
asserts.checkerror('dbquit', msg, 'Nelson:debugger:notInDebugMode');
%=============================================================================
% dbstatus with a file filter and -completenames
dbstop in bpl at 2
b = dbstatus('bpl');
asserts.isequal(b.name, 'bpl');
b = dbstatus('-completenames');
asserts.isequal(b.name, [which('bpl'), '>bpl']);
txt = evalc('dbstatus -completenames');
asserts.isequal(txt, ['Breakpoint for ', which('bpl'), '>bpl is on line 2.', LF]);
txt = evalc('dbstatus bpl');
asserts.isequal(txt, ['Breakpoint for bpl is on line 2.', LF]);
dbclear all
%=============================================================================
