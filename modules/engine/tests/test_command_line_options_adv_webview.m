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
nelson_adv_cli = [binpath, '/nelson-adv-cli'];
%=============================================================================
% On the advanced terminal '--webview' is a real option (web graphics backend),
% not a mode selector: the command is accepted and the REPL keeps running.
[status, output] = run_nelson_cli(nelson_adv_cli, ' --webview --quiet --no-user-startup --no-ipc -e "quit"');
asserts.isequal(status, 0);
%=============================================================================
