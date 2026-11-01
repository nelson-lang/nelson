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
% dbstop if caught error seen from the editor: raising line shown, dbcont resumes in the catch.
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NASGU>
fixture = fullfile(fixtureDirectory, 'editor_err_fun.m');
filewrite(fixture, ['function r = editor_err_fun(x)', LF, ...
  '  r = x + 1;', LF, ...
  '  r = undefined_editor_thing + r;', LF, ...
  'end', LF]);
addpath(fixtureDirectory);
pathCleanup = onCleanup(@() rmpath(fixtureDirectory)); %#ok<NASGU>
editorCleanup = onCleanup(@() __editor__('close')); %#ok<NASGU>
global editor_test_log
editor_test_log = {};
dbclear all
__editor__('open', fixture);
dbstop if caught error
__editor__('queueCommand', 'global editor_test_log; editor_test_log{end+1} = __editor__(''debugState'');');
__editor__('queueCommand', 'global editor_test_log; editor_test_log{end+1} = __editor__(''hover'', 2, 3);');
__editor__('queueCommand', '__editor__(''command'', ''continue'');');
caught = '';
try
  editor_err_fun(1);
catch e
  caught = e.message;
end
asserts.istrue(contains(caught, 'undefined_editor_thing'));
asserts.isequal(numel(editor_test_log), 2);
state = editor_test_log{1};
asserts.istrue(state.paused);
asserts.isequal(state.reason, 'error');
asserts.isequal(state.line, 3);
asserts.istrue(strcmp(strrep(state.file, '\', '/'), strrep(fixture, '\', '/')));
asserts.istrue(contains(state.errorMessage, 'undefined_editor_thing'));
asserts.isequal(editor_test_log{2}, 'r = 2');
dbclear all
clear global editor_test_log
% release the cleanups here, not at process exit, where the editor is already shutting down
clear editorCleanup
clear pathCleanup
clear fixtureDirectoryCleanup
%=============================================================================
