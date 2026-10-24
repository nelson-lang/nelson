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
% Markers are recorded as one "points" batch; the vector exports must still
% write one shape per marker.
out = [tempdir(), 'saveas_scatter_markers_vector'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
% Filled circles with an outline: one <circle> per point, rgba face and edge.
f = figure('Visible', 'off', 'Position', [100 100 420 315], 'Color', [1 1 1]);
scatter(1:20, mod(1:20, 7), 36, 'filled', 'MarkerFaceColor', [1 0 0], 'MarkerEdgeColor', [0 0 1]);
drawnow();
svgFile = [out, '/circles.svg'];
saveas(f, svgFile);
svgInfo = assert_valid_svg(svgFile, [420 315], 'circle');
asserts.isequal(numel(regexp(svgInfo.text, '<circle ', 'match')), 20);
asserts.istrue(contains(svgInfo.text, 'fill="rgba(255,0,0,1.000000)"'));
asserts.istrue(contains(svgInfo.text, 'stroke="rgba(0,0,255,1.000000)"'));
rmfile(svgFile);
% PDF: a disc is four Bezier arcs, filled and stroked with the same colours.
pdfFile = [out, '/circles.pdf'];
saveas(f, pdfFile);
pdfText = fileread(pdfFile);
asserts.isequal(numel(strfind(pdfText, sprintf(' c\n'))), 80);
asserts.istrue(contains(pdfText, '1 0 0 rg'));
asserts.istrue(contains(pdfText, '0 0 1 RG'));
asserts.isequal(numel(strfind(pdfText, sprintf('h B\n'))), 20);
rmfile(pdfFile);
close(f);
%=============================================================================
% Hollow squares: one <polygon> per point without a fill.
f = figure('Visible', 'off', 'Position', [100 100 420 315], 'Color', [1 1 1]);
scatter(1:10, 1:10, 36, 's');
drawnow();
svgFile = [out, '/squares.svg'];
saveas(f, svgFile);
svgInfo = assert_valid_svg(svgFile, [420 315], 'polygon');
asserts.isequal(numel(regexp(svgInfo.text, '<polygon [^>]*fill="none"', 'match')), 10);
rmfile(svgFile);
close(f);
%=============================================================================
% Plus markers: two strokes per point in the marker colour.
f = figure('Visible', 'off', 'Position', [100 100 420 315], 'Color', [1 1 1]);
plot(1:10, 1:10, 'r+');
drawnow();
svgFile = [out, '/plus.svg'];
saveas(f, svgFile);
svgInfo = assert_valid_svg(svgFile, [420 315], 'line');
asserts.isequal(numel(regexp(svgInfo.text, '<line [^>]*stroke="rgba\(255,0,0,1\.000000\)"', 'match')), 20);
rmfile(svgFile);
close(f);
%=============================================================================
% Too many filled markers for a vector file: the whole figure becomes a raster.
f = figure('Visible', 'off', 'Position', [100 100 420 315], 'Color', [1 1 1]);
scatter(rand(1, 3000), rand(1, 3000), 10, 'filled');
drawnow();
svgFile = [out, '/raster.svg'];
saveas(f, svgFile);
svgInfo = assert_valid_svg(svgFile, [420 315], 'image');
asserts.isequal(numel(regexp(svgInfo.text, '<circle ', 'match')), 0);
rmfile(svgFile);
close(f);
%=============================================================================
