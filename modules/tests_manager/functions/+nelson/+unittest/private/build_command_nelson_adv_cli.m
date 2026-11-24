%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cmd = build_command_nelson_adv_cli(command, withMPI)
  % `--headless`: batch test runs render figures offscreen (no window), the stable
  % path for the suite. The advanced terminal otherwise renders real web windows.
  % NOTE: the launcher is NOT flipped to render mode yet. The render-mode gate
  % (measured 2026-11-24) is negative: 5 graphics tests that pass offscreen fail
  % under the web raster path (axes_auto_font_size, contourslice, legend_displayname,
  % scatter3, uicontainer_axes_idle_drawnow). Flipping waits on the render-fidelity
  % engine work (sibling sessions s7/s3), per the plan's gate.
  cmd = build_command_nelson_mode('nelson-adv-cli', ['--headless ', command], withMPI);
end
