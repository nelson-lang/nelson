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
out = [tempdir(), 'saveas_svg_matches_getframe_patch'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [1 1 1]);
patch('XData', [-1 1 1 -1], 'YData', [-1 -1 1 1], 'ZData', [0 0 0 0], ...
  'FaceColor', [0.1 0.45 0.9], 'EdgeColor', 'k', 'LineWidth', 1.5);
hold on;
patch('XData', [-1 1 1 -1], 'YData', [0 0 0 0], 'ZData', [-1 -1 1 1], ...
  'FaceColor', [0.9 0.35 0.1], 'EdgeColor', 'k', 'LineWidth', 1.5);
axis([-1.2 1.2 -1.2 1.2 -1.2 1.2]);
view(35, 24);
drawnow();
svgFile = [out, '/patch_intersections.svg'];
saveas(f, svgFile);
assert_valid_svg(svgFile, [620 460], 'polygon|image');
close(f);
%=============================================================================
