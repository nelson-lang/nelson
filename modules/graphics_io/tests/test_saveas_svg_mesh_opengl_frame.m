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
addpath([modulepath('graphics', 'tests')]);
out = [tempdir(), 'saveas_svg_mesh_opengl_frame'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [1 1 1]);
[X, Y] = meshgrid(-8:.5:8);
R = sqrt(X .^ 2 + Y .^ 2) + eps;
Z = sin(R) ./ R;
mesh(X, Y, Z);
axis square;
drawnow();
pause(0.1);
svgFile = [out, '/mesh_axis_square.svg'];
saveas(f, svgFile);
drawnow();
assert_valid_svg(svgFile, [620 460], 'polygon|image');
close(f);
drawnow();
pause(0.1);
%=============================================================================
