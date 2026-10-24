%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
addpath([modulepath('graphics_io'), '/tests/helpers']);
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
addpath([modulepath('graphics', 'tests')]);
out = [tempdir(), 'saveas_svg_matches_getframe_bar3'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [1 1 1]);
Y = [1 2 3; 4 5 6];
bar3(Y);
drawnow();
svgFile = [out, '/bar3_matrix.svg'];
saveas(f, svgFile);
assert_valid_svg(svgFile, [620 460], 'polygon|image');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 1074 647], 'Color', [1 1 1]);
bar3(Y);
drawnow();
svgFile = [out, '/bar3_matrix_large.svg'];
saveas(f, svgFile);
assert_valid_svg(svgFile, [1074 647], 'polygon|image');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 1074 647], 'Color', [1 1 1]);
z = [1 4 7; 2 5 8; 3 6 9; 4 7 10];
bar3(z);
drawnow();
svgFile = [out, '/bar3_depth_case.svg'];
saveas(f, svgFile);
assert_valid_svg(svgFile, [1074 647], 'polygon|image');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Color', [1 1 1]);
bar3(z);
drawnow();
svgFile = [out, '/bar3_user_case.svg'];
saveas(f, svgFile);
assert_valid_svg(svgFile, [], 'polygon|image');
close(f);
%=============================================================================
