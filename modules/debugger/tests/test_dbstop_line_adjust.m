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
% Breakpoint line adjustment: comments, blank and function lines move to the next statement.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NLS0004>
filewrite(fullfile(fixtureDirectory, 'bpl.m'), ['function y = bpl(x)', LF, ...
  '% c', LF, ...
  '', LF, ...
  'y = 0;', LF, ...
  'if x > 0', LF, ...
  '  y = 1;', LF, ...
  'else', LF, ...
  '  y = 2;', LF, ...
  'end', LF, ...
  'for k=1:2', LF, ...
  '  y = y + k;', LF, ...
  'end', LF, ...
  'end', LF, ...
  'function z = loc(a)', LF, ...
  'z = a;', LF, ...
  'end', LF]);
addpath(fixtureDirectory);
pathCleanup = onCleanup(@() rmpath(fixtureDirectory)); %#ok<NLS0004>
dbclear all
%=============================================================================
% requested line -> [adjusted line, name]; else and end of if move to the next statement
expected = {1, 4, 'bpl'; 2, 4, 'bpl'; 3, 4, 'bpl'; 4, 4, 'bpl'; 5, 5, 'bpl'; 6, 6, 'bpl'; ...
  7, 8, 'bpl'; 8, 8, 'bpl'; 9, 10, 'bpl'; 10, 10, 'bpl'; 11, 11, 'bpl'; 12, 12, 'bpl'; ...
  13, 13, 'bpl'; 14, 15, 'bpl>loc'; 15, 15, 'bpl>loc'; 16, 16, 'bpl>loc'; 0, 4, 'bpl'};
for i = 1:size(expected, 1)
  dbclear all
  eval(sprintf('dbstop in bpl at %d', expected{i, 1}));
  b = dbstatus();
  asserts.isequal(b.line, expected{i, 2}, sprintf('requested line %d', expected{i, 1}));
  asserts.isequal(b.name, expected{i, 3}, sprintf('requested line %d', expected{i, 1}));
end
dbclear all
%=============================================================================
% past the last expression: refused
asserts.checkerror('dbstop in bpl at 99', _('Setting or clearing breakpoints past the start of the last expression is not supported.'), 'Nelson:dbstop:invalidArgument');
asserts.istrue(isempty(dbstatus()));
%=============================================================================
% dbstop in file: first executable line
dbstop in bpl
b = dbstatus();
asserts.isequal(b.line, 4);
dbclear all
%=============================================================================
