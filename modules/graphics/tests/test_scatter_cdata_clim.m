%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
% Scalar marker CData is mapped with the axes CLim, not with the data range.
%=============================================================================
close all
%=============================================================================
function [p1, p2] = markerPixels(f)
  drawnow();
  g = getframe(f);
  c = double(g.cdata);
  r = round(size(c, 1) / 2);
  p1 = squeeze(c(r, round(size(c, 2) / 3), :))';
  p2 = squeeze(c(r, round(2 * size(c, 2) / 3), :))';
end
%=============================================================================
% hidden figure: software render path
f = figure('Visible', 'off', 'Position', [10 10 200 200]);
ax = axes(f);
scatter(ax, [1 2], [1 1], 2000, [10 20], 'filled');
colormap(f, gray(256));
caxis(ax, [0 100]);
xlim(ax, [0 3]);
asserts.isequal(get(ax, 'CLim'), [0 100]);
asserts.isequal(get(ax, 'CLimMode'), 'manual');
[p1, p2] = markerPixels(f);
asserts.isequal(p1, [25 25 25]);
asserts.isequal(p2, [51 51 51]);
% a new CLim recolors the markers
caxis(ax, [0 40]);
[p1, p2] = markerPixels(f);
asserts.isequal(p1, [64 64 64]);
asserts.isequal(p2, [128 128 128]);
% auto CLim spans the data
caxis(ax, 'auto');
asserts.isequal(get(ax, 'CLim'), [10 20]);
[p1, p2] = markerPixels(f);
asserts.isequal(p1, [0 0 0]);
asserts.isequal(p2, [255 255 255]);
caxis(ax, [0 100]);
% svg export uses the same colors
svgFile = [tempname(), '.svg'];
saveas(f, svgFile);
txt = fileread(svgFile);
delete(svgFile);
asserts.istrue(contains(txt, 'rgba(25,25,25,'));
asserts.istrue(contains(txt, 'rgba(51,51,51,'));
% a uicontrol on a hidden figure selects the web raster path
uicontrol(f, 'Style', 'text', 'String', '', 'Position', [1 1 5 5]);
[p1, p2] = markerPixels(f);
asserts.isequal(p1, [25 25 25]);
asserts.isequal(p2, [51 51 51]);
close(f);
%=============================================================================
% scatter3 seen from above
f = figure('Visible', 'off', 'Position', [10 10 200 200]);
ax = axes(f);
scatter3(ax, [1 2], [1 1], [0 0], 2000, [10 20], 'filled');
view(ax, 2);
colormap(f, gray(256));
caxis(ax, [0 100]);
xlim(ax, [0 3]);
[p1, p2] = markerPixels(f);
asserts.isequal(p1, [25 25 25]);
asserts.isequal(p2, [51 51 51]);
close(f);
%=============================================================================
% CLim set before the scatter call
f = figure('Visible', 'off', 'Position', [10 10 200 200]);
ax = axes(f);
colormap(f, gray(256));
caxis(ax, [0 100]);
hold(ax, 'on');
scatter(ax, [1 2], [1 1], 2000, [10 20], 'filled');
xlim(ax, [0 3]);
asserts.isequal(get(ax, 'CLim'), [0 100]);
[p1, p2] = markerPixels(f);
asserts.isequal(p1, [25 25 25]);
asserts.isequal(p2, [51 51 51]);
close(f);
%=============================================================================
% bubblechart follows the same rule
f = figure('Visible', 'off', 'Position', [10 10 200 200]);
ax = axes(f);
b = bubblechart(ax, [1 2], [1 1], [1 1], [10 20]);
b.MarkerFaceAlpha = 1;
bubblesize(ax, [39 40]);
colormap(f, gray(256));
caxis(ax, [0 100]);
xlim(ax, [0 3]);
[p1, p2] = markerPixels(f);
asserts.isequal(p1, [25 25 25]);
asserts.isequal(p2, [51 51 51]);
close(f);
%=============================================================================
% a column of three scalars is colormapped, not an RGB triplet
f = figure('Visible', 'off', 'Position', [10 10 300 200]);
ax = axes(f);
scatter(ax, [1 2 3], [1 1 1], 2000, [10; 20; 30], 'filled');
colormap(f, gray(256));
xlim(ax, [0 4]);
asserts.isequal(get(ax, 'CLim'), [10 30]);
drawnow();
g = getframe(f);
c = double(g.cdata);
r = round(size(c, 1) / 2);
w = size(c, 2);
asserts.isequal(squeeze(c(r, round(w / 4), :))', [0 0 0]);
asserts.isequal(squeeze(c(r, round(w / 2), :))', [128 128 128]);
asserts.isequal(squeeze(c(r, round(3 * w / 4), :))', [255 255 255]);
close(f);
%=============================================================================
