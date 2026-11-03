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
% The code analyzer parses the open file on a worker thread while the prompt evaluates commands.
%=============================================================================
LF = char(10);
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
fixtureDirectoryCleanup = onCleanup(@() rmdir(fixtureDirectory, 's')); %#ok<NLS0004>
fixture = fullfile(fixtureDirectory, 'editor_big_fun.m');
body = cell(1, 4000);
for k = 1:4000
  body{k} = sprintf('  a%d = [x, %d; %d, x] * (x + %d) - sum(1:%d);', mod(k, 50), k, k, k, k);
end
filewrite(fixture, ['function r = editor_big_fun(x)', LF, strjoin(body, LF), LF, '  r = x;', LF, 'end', LF]);
editorCleanup = onCleanup(@() __editor__('close')); %#ok<NLS0004>
__editor__('open', fixture);
for i = 1:5
  % an edit restarts the analysis, the pause lets it start
  __editor__('insertLine', 2, sprintf('  b = %d;', i));
  pause(0.6);
  for k = 1:300
    eval(sprintf('z = [%d, 2; 3, 4] * %d;', k, i)); %#ok<NLS0022>
  end
  asserts.isequal(z, [k, 2; 3, 4] * i);
end
%=============================================================================
% release the cleanups here, not at process exit, where the editor is already shutting down
clear editorCleanup
clear fixtureDirectoryCleanup
%=============================================================================
