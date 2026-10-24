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
%=============================================================================
% 'axis off' on a polar axes hides its ruler -- the grid lines, tick labels and
% the white background disc -- leaving only the plotted data, as the reference
% does. 'axis on' brings the ruler back. The polar ruler is simulated with
% child objects plus the axes Color, so these must track the on/off state.
%=============================================================================
theta = 0:0.05:2 * pi;
rho = abs(sin(2 * theta));
%=============================================================================
% The figure is black so the white background disc is the discriminator: with
% the ruler on it covers the centre, with 'axis off' it is gone and the black
% figure shows through.
%=============================================================================
% A normal polar plot shows the white disc and grid: plenty of near-white pixels.
f = figure('Visible', 'off', 'Color', 'k', 'Position', [100 100 300 300]);
polarplot(theta, rho);
drawnow();
png = [tempdir(), 'test_polaraxes_axis_off_on.png'];
print(f, png, '-dpng');
onImg = imread(png);
whiteOn = sum(onImg(:, :, 1) > 200 & onImg(:, :, 2) > 200 & onImg(:, :, 3) > 200, 'all');
asserts.istrue(whiteOn > 1000);
close(f);
%=============================================================================
% 'axis off' removes the disc and grid: the black figure shows through, so the
% near-white pixels collapse.
f = figure('Visible', 'off', 'Color', 'k', 'Position', [100 100 300 300]);
polarplot(theta, rho);
hold on;
axis off;
drawnow();
png = [tempdir(), 'test_polaraxes_axis_off_off.png'];
print(f, png, '-dpng');
offImg = imread(png);
whiteOff = sum(offImg(:, :, 1) > 200 & offImg(:, :, 2) > 200 & offImg(:, :, 3) > 200, 'all');
asserts.istrue(whiteOff < whiteOn / 10);
close(f);
%=============================================================================
% 'axis on' restores the ruler.
f = figure('Visible', 'off', 'Color', 'k', 'Position', [100 100 300 300]);
polarplot(theta, rho);
hold on;
axis off;
axis on;
drawnow();
png = [tempdir(), 'test_polaraxes_axis_off_back.png'];
print(f, png, '-dpng');
backImg = imread(png);
whiteBack = sum(backImg(:, :, 1) > 200 & backImg(:, :, 2) > 200 & backImg(:, :, 3) > 200, 'all');
asserts.istrue(whiteBack > 1000);
close(f);
%=============================================================================
