%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
binpath = modulepath('nelson', 'bin');
if ispc()
  nelson_adv_cli = [binpath, '/NelSon-adv-cli.exe'];
  banner_cmd = ['echo exit | "', nelson_adv_cli, '" --no-ipc --no-user-modules'];
else
  nelson_adv_cli = [binpath, '/nelson-adv-cli'];
  banner_cmd = ['printf "exit\n" | "', nelson_adv_cli, '" --no-ipc --no-user-modules'];
end
skip_testsuite(~isfile(nelson_adv_cli), ['Executable not found: ', nelson_adv_cli]);

[status, msg] = system(banner_cmd);
asserts.isequal(status, 0);
asserts.istrue(contains(msg, '_   __'));
asserts.isfalse(contains(msg, 'input arguments.'));

nelson_cmd = 'disp(1+1); exit';
cmd = ['"', nelson_adv_cli, '" --no-ipc --no-user-modules --quiet --execute "', nelson_cmd, '"'];
[status, msg] = system(cmd);
asserts.isequal(status, 0);
asserts.istrue(contains(msg, '2'));
asserts.isfalse(contains(msg, 'input arguments.'));
%=============================================================================
