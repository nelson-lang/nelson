%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
% opentoline places the cursor of the editor at a line, and at a column when given.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NLS0004>
fixture = fullfile(fixtureDirectory, 'opentoline_fixture.m');
filewrite(fixture, ['function r = opentoline_fixture(x)', LF, '  r = x + 1;', LF, '  r = r * 2;', LF, 'end', LF]);
editorCleanup = onCleanup(@() __editor__('close')); %#ok<NLS0004>
opentoline(fixture, 2);
asserts.istrue(strcmp(strrep(__editor__('currentFile'), '\', '/'), strrep(fixture, '\', '/')));
asserts.isequal(__editor__('cursor'), [2 1]);
opentoline(fixture, 3, 7);
asserts.isequal(__editor__('cursor'), [3 7]);
% a column past the end of the line stops at the end of the line
opentoline(fixture, 2, 200);
asserts.isequal(__editor__('cursor'), [2 13]);
% the arguments are checked before the editor is touched
assert_checkerror('opentoline(fixture)', _('Wrong number of input arguments.'));
assert_checkerror('opentoline(3, 2)', _('A file name is expected as first argument.'));
assert_checkerror('opentoline(fixture, 0)', _('A line number of 1 or more is expected as second argument.'));
assert_checkerror('opentoline(fixture, 2, 0)', _('A column number of 1 or more is expected as third argument.'));
% release the cleanups here, not at process exit, where the editor is already shutting down
clear editorCleanup
clear fixtureDirectoryCleanup
%=============================================================================
