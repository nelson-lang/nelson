%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function command = nelson_tool_command_for_test(toolName)
  binpath = modulepath('nelson', 'bin');
  toolPath = [binpath, '/', toolName];
  % On Windows the tool carries a '.exe' suffix that the command line resolves
  % implicitly, so probe both spellings before deciding to skip.
  available = isfile(toolPath) || (ispc() && isfile([toolPath, '.exe']));
  skip_testsuite(~available, [toolName, ' command-line tool is not available.']);
  command = ['"', toolPath, '"'];
end
%=============================================================================
