%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% "-e=command" runs the command itself, not "=command".
binpath = modulepath('nelson', 'bin');
nelson_cli = [binpath, '/nelson-cli'];
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --no-user-startup --no-ipc -e="disp(4242)"');
asserts.isequal(status, 0, output);
asserts.istrue(contains(output, '4242'), output);
%=============================================================================
if ispc()
  [status, output] = run_nelson_cli(nelson_cli, ' --quiet --no-user-startup --no-ipc /e="disp(4343)"');
  asserts.isequal(status, 0, output);
  asserts.istrue(contains(output, '4343'), output);
end
%=============================================================================
