%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% nelson-adv-cli with the web graphics backend (`--webview`) for the native
% process launcher used by the parallel/progressive runners.
function [executable, processArguments] = build_native_nelson_adv_cli_webview(command, withMPI)
  [executable, processArguments] = build_native_nelson_mode('nelson-adv-cli', ['--webview ', command], withMPI); %#ok<NLS0004>
end
