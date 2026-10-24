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
out = [tempdir(), 'saveas_svg_matches_getframe_uicontrol'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [1 1 1]);
surf(peaks(24), 'EdgeColor', 'none');
hold on;
plot3(1:24, 12 * ones(1, 24), linspace(-8, 8, 24), 'k-', 'LineWidth', 2);
uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'Export', ...
  'Position', [30 30 90 28]);
drawnow();
svgFile = [out, '/surface_plot_uicontrol.svg'];
saveas(f, svgFile);
svgInfo = assert_valid_svg(svgFile, [620 460], 'polygon|image');
if contains(svgInfo.text, '<image ')
  asserts.istrue(contains(svgInfo.text, 'data:image/png;base64,'));
else
  asserts.istrue(contains(svgInfo.text, 'Export'));
end
close(f);
%=============================================================================
