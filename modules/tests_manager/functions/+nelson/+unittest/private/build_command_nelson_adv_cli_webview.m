%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% nelson-adv-cli with the web graphics backend (`--webview`): same terminal
% REPL, figures rendered headless through RenderWeb instead of Qt.
function cmd = build_command_nelson_adv_cli_webview(command, withMPI)
  cmd = build_command_nelson_mode('nelson-adv-cli', ['--webview ', command], withMPI);
end
