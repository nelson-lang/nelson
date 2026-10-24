%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [status, output] = run_nelson_cli(nelson_cli, arguments, timeout)
  if nargin < 3
    timeout = 30;
  end
  % On Windows the executable carries a '.exe' suffix that the command line
  % resolves implicitly, so probe both spellings before deciding to skip.
  cli_available = isfile(nelson_cli) || (ispc() && isfile([nelson_cli, '.exe']));
  skip_testsuite(~cli_available, ['Executable not found: ', nelson_cli]);
  quoted_command = ['"', replace(nelson_cli, '"', '\"'), '"'];
  [status, output] = system([quoted_command, arguments], timeout);
end
%=============================================================================
