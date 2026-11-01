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
% Gutter breakpoints: line adjustment, conditions, enable/disable, dbstop/dbclear follow-up.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
fixture = fullfile(fixtureDirectory, 'editor_bp_fun.m');
filewrite(fixture, ['function r = editor_bp_fun(x)', LF, ...
  '  % comment', LF, ...
  '  r = x + 1;', LF, ...
  '  for k = 1:2', LF, ...
  '    r = r + k;', LF, ...
  '  end', LF, ...
  'end', LF]);
addpath(fixtureDirectory);
pathCleanup = onCleanup(@() rmpath(fixtureDirectory)); %#ok<NASGU>
editorCleanup = onCleanup(@() __editor__('close')); %#ok<NASGU>
dbclear all
__editor__('open', fixture);
asserts.istrue(strcmp(__editor__('currentFile'), fixture));
%=============================================================================
% a click on a comment line moves to the next executable line
__editor__('toggleBreakpoint', 2);
b = dbstatus();
asserts.isequal(b.name, 'editor_bp_fun');
asserts.isequal(b.line, 3);
e = __editor__('breakpoints');
asserts.isequal(e.line, 3);
asserts.istrue(e.enabled);
asserts.isfalse(e.invalid);
asserts.isequal(e.condition, '');
%=============================================================================
% toggling the marker again removes it
__editor__('toggleBreakpoint', 3);
asserts.istrue(isempty(dbstatus()));
asserts.istrue(isempty(__editor__('breakpoints')));
%=============================================================================
% the `end` of the loop is a breakpoint line of its own
__editor__('toggleBreakpoint', 6);
b = dbstatus();
asserts.isequal(b.line, 6);
__editor__('toggleBreakpoint', 6);
%=============================================================================
% conditions set from the editor
__editor__('toggleBreakpoint', 5);
r = __editor__('setCondition', 5, 'k == 2');
asserts.istrue(r);
b = dbstatus();
asserts.isequal(b.line, 5);
asserts.isequal(b.expression, {'k == 2'});
[r, msg] = __editor__('setCondition', 5, 'k >>> 2');
asserts.isfalse(r);
asserts.isequal(msg, _('The condition ''k >>> 2'' contains a syntax error.'));
b = dbstatus();
asserts.isequal(b.expression, {'k == 2'});
e = __editor__('breakpoints');
asserts.isequal(e.condition, 'k == 2');
%=============================================================================
% disabled breakpoints stay listed but are not armed
asserts.istrue(__editor__('setEnabled', 5, false));
e = __editor__('breakpoints');
asserts.isfalse(e.enabled);
b = dbstatus();
asserts.isequal(b.line, 5);
asserts.istrue(__editor__('setEnabled', 5, true));
e = __editor__('breakpoints');
asserts.istrue(e.enabled);
%=============================================================================
% the gutter follows the console
dbstop in editor_bp_fun at 3
__editor__('debugState');
e = __editor__('breakpoints');
asserts.isequal([e.line], [3 5]);
dbclear all
__editor__('debugState');
asserts.istrue(isempty(__editor__('breakpoints')));
% release the cleanups here, not at process exit, where the editor is already shutting down
clear editorCleanup
clear pathCleanup
clear fixtureDirectoryCleanup
%=============================================================================
