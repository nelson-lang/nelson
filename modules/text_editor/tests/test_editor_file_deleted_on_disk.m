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
%=============================================================================
% A file open in the editor and deleted on disk keeps its text, without a reload question.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixture = fullfile(fixtureDirectory, 'editor_deleted_fun.m');
filewrite(fixture, ['function r = editor_deleted_fun(x)', LF, '  r = x;', LF, 'end', LF]);
__editor__('open', fixture);
delete(fixture);
asserts.isfalse(isfile(fixture));
% the file watcher reports the deletion: no modal question may block the prompt
for k = 1:10
  drawnow();
  pause(0.05);
end
state = __editor__('debugState');
asserts.isfalse(state.paused);
rmdir(fixtureDirectory, 's');
for k = 1:10
  drawnow();
  pause(0.05);
end
asserts.isfalse(isfolder(fixtureDirectory));
% the process exits with the editor still open on the deleted file
%=============================================================================
