%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% Breakpoints follow the edited lines, are invalid until saved, then reach the evaluator.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
fixture = fullfile(fixtureDirectory, 'editor_edit_fun.m');
filewrite(fixture, ['function r = editor_edit_fun(x)', LF, ...
  '  r = x + 1;', LF, ...
  '  r = r * 2;', LF, ...
  'end', LF]);
addpath(fixtureDirectory);
pathCleanup = onCleanup(@() rmpath(fixtureDirectory)); %#ok<NASGU>
editorCleanup = onCleanup(@() __editor__('close')); %#ok<NASGU>
dbclear all
__editor__('open', fixture);
__editor__('toggleBreakpoint', 3);
b = dbstatus();
asserts.isequal(b.line, 3);
%=============================================================================
% inserting a line above moves the marker and makes it invalid until saved
__editor__('insertLine', 2, '  disp(x);');
e = __editor__('breakpoints');
asserts.isequal(e.line, 4);
asserts.istrue(e.invalid);
% the evaluator still has the breakpoint of the saved file
b = dbstatus();
asserts.isequal(b.line, 3);
%=============================================================================
% a marker added while editing waits for the save
__editor__('toggleBreakpoint', 2);
e = __editor__('breakpoints');
asserts.isequal([e.line], [2 4]);
asserts.istrue(all([e.invalid]));
b = dbstatus();
asserts.isequal(b.line, 3);
%=============================================================================
% saving pushes the moved and the new marker to the evaluator
asserts.istrue(__editor__('save'));
e = __editor__('breakpoints');
asserts.isequal([e.line], [2 4]);
asserts.isfalse(any([e.invalid]));
b = dbstatus();
asserts.isequal(b.line, [2 4]);
content = fileread(fixture);
asserts.istrue(contains(content, 'disp(x);'));
dbclear all
% release the cleanups here, not at process exit, where the editor is already shutting down
clear editorCleanup
clear pathCleanup
clear fixtureDirectoryCleanup
%=============================================================================
