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
out = [tempdir(), 'saveas_histogram2_svg'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
x = [1 1 2 3 4 4];
y = [1 2 2 3 3 4];
histogram2(x, y, [0 2 4], [0 2 4]);
drawnow();
assertSavedFigure(f, out, 'histogram2_1');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
rng(0);
x = randn(400, 1);
y = 0.5 * x + randn(400, 1);
histogram2(x, y, [12 10], 'Normalization', 'probability');
drawnow();
assertSavedFigure(f, out, 'histogram2_2');
close(f);
%=============================================================================
function assertSavedFigure(f, out, name)
  svgFile = [out, '/', name, '.svg'];
  saveas(f, svgFile);
  % The 3D bars export as vector polygons (few enough to stay vector) while the
  % axes and labels stay crisp text; a very dense histogram would flatten to a
  % single raster instead. Either way the plotted data is present as a filled
  % primitive.
  svgInfo = assert_valid_svg(svgFile, [560 420], 'polygon|image');
  svgText = svgInfo.text;
  asserts.istrue(contains(svgText, '<polygon') || contains(svgText, '<image '));
end
%=============================================================================
