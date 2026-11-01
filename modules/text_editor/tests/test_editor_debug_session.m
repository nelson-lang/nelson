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
% A debug session seen from the editor; the K>> commands are queued before the call.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
fixture = fullfile(fixtureDirectory, 'editor_dbg_fun.m');
filewrite(fixture, ['function r = editor_dbg_fun(x)', LF, ...
  '  s.a = x * 2;', LF, ...
  '  r = s.a + 1;', LF, ...
  '  r = r * 2;', LF, ...
  'end', LF]);
addpath(fixtureDirectory);
pathCleanup = onCleanup(@() rmpath(fixtureDirectory)); %#ok<NASGU>
editorCleanup = onCleanup(@() __editor__('close')); %#ok<NASGU>
global editor_test_log
editor_test_log = {};
dbclear all
__editor__('open', fixture);
__editor__('toggleBreakpoint', 3);
b = dbstatus();
asserts.isequal(b.line, 3);
%=============================================================================
% paused on line 3: state, data tips for a variable, a field and a selection
__editor__('queueCommand', 'global editor_test_log; editor_test_log{end+1} = __editor__(''debugState'');');
__editor__('queueCommand', 'global editor_test_log; editor_test_log{end+1} = __editor__(''hover'', 2, 3);');
__editor__('queueCommand', 'global editor_test_log; editor_test_log{end+1} = __editor__(''hover'', 3, 9);');
__editor__('queueCommand', 'global editor_test_log; editor_test_log{end+1} = __editor__(''hover'', 2, 10);');
__editor__('queueCommand', '__editor__(''command'', ''step'');');
__editor__('queueCommand', 'global editor_test_log; editor_test_log{end+1} = __editor__(''debugState'');');
__editor__('queueCommand', '__editor__(''command'', ''continue'');');
y = editor_dbg_fun(5);
asserts.isequal(y, 22);
asserts.isequal(numel(editor_test_log), 5);
state = editor_test_log{1};
asserts.istrue(state.paused);
asserts.istrue(strcmp(strrep(state.file, '\', '/'), strrep(fixture, '\', '/')));
asserts.isequal(state.line, 3);
asserts.isequal(state.reason, 'breakpoint');
asserts.istrue(state.canStep);
asserts.isequal(state.executionLine, 3);
% `s` on line 2, column 3
asserts.istrue(startsWith(editor_test_log{2}, 's = 1'));
asserts.istrue(endsWith(editor_test_log{2}, 'struct (a)'));
% `s.a` on line 3, column 9
asserts.isequal(editor_test_log{3}, 's.a = 10');
% `x` on line 2, column 10
asserts.isequal(editor_test_log{4}, 'x = 5');
state = editor_test_log{5};
asserts.istrue(state.paused);
asserts.isequal(state.line, 4);
asserts.isequal(state.reason, 'step');
%=============================================================================
% after the run the editor shows nothing paused
state = __editor__('debugState');
asserts.isfalse(state.paused);
asserts.isequal(state.executionLine, -1);
dbclear all
clear global editor_test_log
% release the cleanups here, not at process exit, where the editor is already shutting down
clear editorCleanup
clear pathCleanup
clear fixtureDirectoryCleanup
%=============================================================================
