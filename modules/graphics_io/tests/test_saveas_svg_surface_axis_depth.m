%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
addpath([modulepath('graphics_io'), '/tests/helpers']);
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
out = [tempdir(), 'saveas_svg_surface_axis_depth'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Position', [100 100 620 460], 'Color', [1 1 1]);
surf(peaks, 'EdgeColor', 'none');
colormap(jet(64));
drawnow();
svgFile = [out, '/surface_axis_depth.svg'];
saveas(f, svgFile);
assert_valid_svg(svgFile, [620 460], 'polygon|image');
close(f);
%=============================================================================
