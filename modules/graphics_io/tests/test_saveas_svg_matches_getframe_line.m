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
out = [tempdir(), 'saveas_svg_matches_getframe_line'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [1 1 1]);
x = -pi:pi / 10:pi;
y = tan(sin(x)) - sin(tan(x));
plot(x, y, '--rs', 'LineWidth', 2, 'MarkerEdgeColor', 'k', 'MarkerFaceColor', 'g', ...
  'MarkerSize', 11);
drawnow();
svgFile = [out, '/line.svg'];
saveas(f, svgFile);
assert_valid_svg(svgFile, [620 460], {'polyline', 'polygon', 'text'});
close(f);
%=============================================================================
