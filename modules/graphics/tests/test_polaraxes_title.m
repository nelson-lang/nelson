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
% A polaraxes title and subtitle are drawn above the theta labels.
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
pax = polaraxes(f, 'Position', [0.15 0.1 0.6 0.6]);
drawnow();
png = [tempdir(), 'test_polaraxes_title_none.png'];
saveas(f, png);
noTitle = imread(png);
t = title(pax, 'right counterclockwise');
s = subtitle(pax, 'sub text');
drawnow();
assert_isequal(char(t.Visible), 'on');
assert_isequal(char(s.Visible), 'on');
yLim = pax.YLim;
% The subtitle sits above the circle, the title above the subtitle.
assert_istrue(s.Position(2) > yLim(2));
assert_istrue(t.Position(2) > s.Position(2));
% Both clear the 90 degrees label drawn at 1.06 times the radius.
assert_istrue(s.Position(2) > 1.06 * yLim(2));
png = [tempdir(), 'test_polaraxes_title_on.png'];
saveas(f, png);
withTitle = imread(png);
% The band above the circle gains dark text pixels.
rows = 1:round(size(withTitle, 1) * 0.25);
darkBefore = sum(sum(noTitle(rows, :, 1) < 100));
darkAfter = sum(sum(withTitle(rows, :, 1) < 100));
assert_istrue(darkAfter > darkBefore + 100);
%=============================================================================
% Visible 'off' hides the polaraxes and turns its title and subtitle off (reference behaviour).
assert_isequal(char(pax.Visible), 'on');
pax.Visible = 'off';
assert_isequal(char(t.Visible), 'off');
assert_isequal(char(s.Visible), 'off');
drawnow();
png = [tempdir(), 'test_polaraxes_title_hidden.png'];
saveas(f, png);
hiddenAxes = imread(png);
assert_isequal(sum(sum(hiddenAxes(:, :, 1) < 100)), 0);
% Showing the axes shows the title and subtitle again.
pax.Visible = 'on';
assert_isequal(char(t.Visible), 'on');
assert_isequal(char(s.Visible), 'on');
drawnow();
saveas(f, png);
shownAgain = imread(png);
assert_istrue(sum(sum(shownAgain(rows, :, 1) < 100)) > darkBefore + 100);
% A title hidden before the axes stays hidden when the axes come back.
t.Visible = 'off';
pax.Visible = 'off';
pax.Visible = 'on';
assert_isequal(char(t.Visible), 'off');
assert_isequal(char(s.Visible), 'on');
% A title shown on a hidden polaraxes is drawn.
t.Visible = 'on';
pax.Visible = 'off';
t.Visible = 'on';
drawnow();
saveas(f, png);
titleOnly = imread(png);
assert_istrue(sum(sum(titleOnly(rows, :, 1) < 100)) > 100);
close(f);
%=============================================================================
% 'axis off' hides the polar disc and grid but keeps the data and the title.
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
polarplot(linspace(0, 2 * pi, 50), ones(1, 50), 'Color', [1 0 0]);
pax = gca();
t = title(pax, 'TTTT');
axis off;
assert_isequal(char(pax.Visible), 'off');
assert_isequal(char(t.Visible), 'on');
drawnow();
png = [tempdir(), 'test_polaraxes_title_axis_off.png'];
saveas(f, png);
axisOff = imread(png);
red = axisOff(:, :, 1) > 200 & axisOff(:, :, 2) < 100;
dark = axisOff(:, :, 1) < 100 & axisOff(:, :, 2) < 100;
assert_istrue(sum(red(:)) > 100);
% Only the title is dark: it lies in the top quarter.
assert_istrue(sum(sum(dark(rows, :))) > 50);
assert_isequal(sum(dark(:)), sum(sum(dark(rows, :))));
% A second hide of the already hidden axes keeps the title.
pax.Visible = 'off';
assert_isequal(char(t.Visible), 'on');
axis on;
assert_isequal(char(pax.Visible), 'on');
assert_isequal(char(t.Visible), 'on');
close(f);
%=============================================================================
% A default polaraxes makes room for its title in a small figure (values of the reference).
f = figure('Visible', 'off', 'Position', [100 100 400 300]);
polarplot(0:0.1:6, 1:61);
pax = gca();
title(pax, 'TTTT');
drawnow();
assert_isapprox(pax.Position(4), 0.753, 2e-3);
axis off;
drawnow();
assert_isapprox(pax.Position(4), 0.753, 2e-3);
png = [tempdir(), 'test_polaraxes_title_small.png'];
saveas(f, png);
small = imread(png);
dark = small(:, :, 1) < 100 & small(:, :, 2) < 100 & small(:, :, 3) < 100;
[darkRows, ~] = find(dark);
% The bold title is whole: its top row is not cut by the figure edge.
assert_istrue(numel(darkRows) > 50);
assert_istrue(min(darkRows) > 2);
close(f);
f = figure('Visible', 'off', 'Position', [100 100 920 543]);
pax = polaraxes(f);
title(pax, 'Title text');
subtitle(pax, 'sub text');
drawnow();
assert_isapprox(pax.Position(4), 0.7769, 2e-3);
f.Position = [100 100 400 300];
drawnow();
assert_isapprox(pax.Position(4), 0.6848, 2e-3);
f.Position = [100 100 920 543];
drawnow();
assert_isapprox(pax.Position(4), 0.7769, 2e-3);
% A tall figure has room above the disc: the box keeps its default height.
f.Position = [100 100 400 600];
drawnow();
assert_isapprox(pax.Position(4), 0.815, 1e-6);
close(f);
%=============================================================================
% Without a subtitle the title takes the subtitle place.
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
pax = polaraxes(f, 'Position', [0.15 0.1 0.6 0.6]);
title(pax, 'with subtitle');
s = subtitle(pax, 'sub text');
drawnow();
subtitleY = s.Position(2);
close(f);
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
pax = polaraxes(f, 'Position', [0.15 0.1 0.6 0.6]);
t = title(pax, 'only title');
drawnow();
assert_istrue(abs(t.Position(2) - subtitleY) < 1e-10);
close(f);
%=============================================================================
