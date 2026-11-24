%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CLI MODE-->
%=============================================================================
skip_if_portable_graphics_backend('advanced CLI launcher is not available.');
%=============================================================================
binpath = modulepath('nelson', 'bin');
% Windows ships the launcher as an .exe; macOS and Linux use the exec wrapper.
if ispc()
  nelson_adv_cli = [binpath, '/nelson-adv-cli.exe'];
else
  nelson_adv_cli = [binpath, '/nelson-adv-cli-exec'];
end
skip_testsuite(~isfile(nelson_adv_cli), _('Advanced CLI launcher not found.'));
quoted_command = ['"', replace(nelson_adv_cli, '"', '\"'), '"'];
command_line = ' --quiet --no-user-startup --no-ipc -e "figure(); quit"';
[status, output] = system([quoted_command, command_line], 30);
asserts.isequal(status, 0);
%=============================================================================
