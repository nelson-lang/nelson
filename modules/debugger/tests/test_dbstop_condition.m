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
% Conditional breakpoints: dbstop ... if expression.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NLS0004>
filewrite(fullfile(fixtureDirectory, 'bpcond.m'), ['function y = bpcond(x)', LF, ...
  'y = x + 1;', LF, ...
  'for k = 1:3', LF, ...
  '  y = y + k;', LF, ...
  'end', LF, ...
  'end', LF]);
addpath(fixtureDirectory);
pathCleanup = onCleanup(@() rmpath(fixtureDirectory)); %#ok<NLS0004>
dbclear all
%=============================================================================
dbstop in bpcond at 2 if x>2
b = dbstatus();
asserts.isequal(fieldnames(b), {'name'; 'file'; 'line'; 'anonymous'; 'expression'; 'cond'; 'identifier'});
asserts.isequal(b.name, 'bpcond');
asserts.istrue(isfile(b.file));
asserts.isequal(b.line, 2);
asserts.isequal(b.anonymous, 0);
asserts.isequal(b.expression, {'x>2'});
asserts.isequal(b.cond, '');
asserts.istrue(isempty(b.identifier));
txt = evalc('dbstatus');
asserts.istrue(contains(txt, 'Breakpoint for bpcond is on line 2.'));
asserts.istrue(contains(txt, '  Breakpoint on line 2 has conditional expression "x>2".'));
%=============================================================================
% a new dbstop on the same line replaces the condition
dbstop in bpcond at 2 if x<0
b = dbstatus();
asserts.isequal(b.line, 2);
asserts.isequal(b.expression, {'x<0'});
dbstop in bpcond at 2
b = dbstatus();
asserts.isequal(b.line, 2);
asserts.isequal(b.expression, {''});
%=============================================================================
% condition without a line: first executable line
dbclear all
dbstop in bpcond if x > 2
b = dbstatus();
asserts.isequal(b.line, 2);
asserts.isequal(b.expression, {'x > 2'});
%=============================================================================
% syntax errors are rejected and no breakpoint is added
dbclear all
asserts.checkerror('dbstop in bpcond at 2 if x>>>2', _('The condition ''x>>>2'' contains a syntax error.'), 'Nelson:dbstop:conditionSyntaxError');
asserts.istrue(isempty(dbstatus()));
asserts.checkerror('dbstop in bpcond at 2 if x = 3', _('The condition ''x = 3'' contains a syntax error.'), 'Nelson:dbstop:conditionSyntaxError');
asserts.istrue(isempty(dbstatus()));
%=============================================================================
% several lines: one condition slot per line, sorted by line
dbstop in bpcond at 4 if k == 2
dbstop in bpcond at 2
b = dbstatus();
asserts.isequal(b.line, [2 4]);
asserts.isequal(b.anonymous, [0 0]);
asserts.isequal(b.expression, {'', 'k == 2'});
txt = evalc('dbstatus');
asserts.istrue(contains(txt, 'Breakpoints for bpcond are on lines 2, 4.'));
asserts.istrue(contains(txt, '  Breakpoint on line 4 has conditional expression "k == 2".'));
%=============================================================================
% round trip through dbstop(b) keeps the conditions
dbclear all
asserts.istrue(isempty(dbstatus()));
dbstop(b)
b2 = dbstatus();
asserts.isequal(b2.line, [2 4]);
asserts.isequal(b2.expression, {'', 'k == 2'});
%=============================================================================
% functional form
dbclear all
dbstop('in', 'bpcond', 'at', '4', 'if', 'k > 1');
b = dbstatus();
asserts.isequal(b.line, 4);
asserts.isequal(b.expression, {'k > 1'});
dbclear all
%=============================================================================
