%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [executable, processArguments] = build_native_nelson_adv_cli(command, withMPI)
  % `--headless`: batch test runs render figures offscreen (no window). The
  % launcher is NOT flipped to render mode yet (render-mode gate negative,
  % see build_command_nelson_adv_cli.m).
  [executable, processArguments] = build_native_nelson_mode('nelson-adv-cli', ['--headless ', command], withMPI); %#ok<NLS0004>
end
