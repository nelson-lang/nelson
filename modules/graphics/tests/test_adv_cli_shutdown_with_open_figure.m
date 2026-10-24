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
skip_testsuite(~ismac(), _('This test requires the macOS GUI runtime.'));
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_adv_cli = [binpath, '/nelson-adv-cli-exec'];
quoted_command = ['"', replace(nelson_adv_cli, '"', '\"'), '"'];
command_line = ' --quiet --no-user-startup --no-ipc -e "figure(); quit"';
[status, output] = system([quoted_command, command_line], 30);
asserts.isequal(status, 0);
%=============================================================================
