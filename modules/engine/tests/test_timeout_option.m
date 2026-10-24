%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% --timeout must be a hard limit: the process must terminate even if the
% interpreter is stuck in a busy loop and cannot cooperate with cleanup.
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_cli = [binpath, '/nelson-cli'];
%=============================================================================
tic();
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --no-user-startup --no-ipc --timeout 2 -e "while true; end"');
elapsed = toc();
if ispc()
  % WAIT_TIMEOUT (258)
  asserts.isequal(status, 258);
else
  % 128 + SIGABRT
  asserts.isequal(status, 134);
end
asserts.istrue(elapsed >= 2);
asserts.istrue(elapsed < 10);
%=============================================================================
