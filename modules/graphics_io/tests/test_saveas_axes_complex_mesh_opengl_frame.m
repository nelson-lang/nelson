%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
out = [tempdir(), 'saveas_axes_complex_mesh_opengl_frame'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [1 1 1]);
F = str2func('@(z) z .^ 3 - 1');
x = linspace(-2, 2, 100);
y = linspace(-2, 2, 100);
[X, Y] = meshgrid(x, y);
Z = X + 1i * Y;
W = F(Z);
mesh(real(W), imag(W), abs(W));
xlabel('Real');
ylabel('Imaginary');
zlabel('Magnitude');
drawnow();
pause(0.1);
svgFile = [out, '/mesh_complex_transform.svg'];
saveas(f, svgFile);
drawnow();
assert_valid_svg(svgFile, [620 460], 'polygon|image');
close(f);
drawnow();
pause(0.1);
%=============================================================================
