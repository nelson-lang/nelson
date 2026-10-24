%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
addpath([modulepath('graphics', 'tests')]);
out = [tempdir(), 'saveas_svg_matches_getframe_surface_plot3'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [1 1 1]);
[xg, yg, zg] = peaks(28);
surf(xg, yg, zg, 'EdgeColor', 'none');
hold on;
t = linspace(min(xg(:)), max(xg(:)), 160);
plot3(t, zeros(size(t)), 8 * sin(t), 'k-', 'LineWidth', 2);
view(42, 28);
drawnow();
svgFile = [out, '/surface_plot3_depth.svg'];
saveas(f, svgFile);
assert_valid_svg(svgFile, [620 460], 'polygon|image');
close(f);
%=============================================================================
