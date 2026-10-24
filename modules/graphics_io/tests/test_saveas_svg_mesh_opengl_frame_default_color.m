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
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
out = [tempdir(), 'saveas_svg_mesh_opengl_frame_default_color'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460]);
[X, Y] = meshgrid(-8:.5:8);
R = sqrt(X .^ 2 + Y .^ 2) + eps;
Z = sin(R) ./ R;
mesh(X, Y, Z);
axis square;
drawnow();
pause(0.1);
beforeColor = get(f, 'Color');
svgFile = [out, '/mesh_axis_square_default_color.svg'];
saveas(f, svgFile);
drawnow();
afterColor = get(f, 'Color');
asserts.istrue(max(abs(beforeColor - afterColor)) <= eps());
asserts.isequal(afterColor, get(groot(), 'DefaultFigureColor'));
close(f);
drawnow();
pause(0.1);
%=============================================================================
