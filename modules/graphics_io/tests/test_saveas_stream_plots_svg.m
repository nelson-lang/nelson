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
out = [tempdir(), 'saveas_stream_plots_svg'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [1 1 1]);
[x, y] = meshgrid(-2:2, -2:2);
streamline(x, y, -y, x, 0, 0);
drawnow();
assertSavedFigure(f, out, 'streamline_1');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [1 1 1]);
t = 0:.15:2;
vertices = {[transpose(cos(t)), transpose(sin(t)), transpose(t)]};
twistangle = {transpose(cos(t))};
streamribbon(vertices, twistangle);
drawnow();
assertSavedFigure(f, out, 'streamribbon_1');
close(f);
%=============================================================================
function assertSavedFigure(f, out, name)
  svgFile = [out, '/', name, '.svg'];
  saveas(f, svgFile);
  assert_valid_svg(svgFile, [620 460], 'line|polygon|image');
end
%=============================================================================
