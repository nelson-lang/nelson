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
% Polar axes follow the same auto FontSize rule as cartesian axes.
%=============================================================================
% Default polaraxes: the outer box drives the size, a title does not shrink it.
sizes = [400 300; 300 220; 250 200; 200 160; 160 130; 600 150; 150 600; 920 543];
expected = [10, 9, 9, 8.5, 8, 8.5, 8, 10];
for k = 1:size(sizes, 1)
  f = figure('Visible', 'off', 'Position', [100 100 sizes(k, :)]);
  pax = polaraxes(f);
  drawnow();
  assert_isequal(pax.FontSize, expected(k));
  title(pax, 'Title text');
  subtitle(pax, 'sub text');
  drawnow();
  assert_isequal(pax.FontSize, expected(k));
  assert_isapprox(pax.Title.FontSize, 1.1 * expected(k), 1e-12);
  close(f);
end
%=============================================================================
% Resizing a titled polaraxes keeps the size of the outer box rule.
f = figure('Visible', 'off', 'Position', [100 100 920 543]);
pax = polaraxes(f);
title(pax, 'Title text');
subtitle(pax, 'sub text');
drawnow();
f.Position = [100 100 400 300];
drawnow();
assert_isequal(pax.FontSize, 10);
assert_isequal(pax.Title.FontSize, 11);
f.Position = [100 100 200 160];
drawnow();
assert_isequal(pax.FontSize, 8.5);
assert_isapprox(pax.Title.FontSize, 9.35, 1e-12);
close(f);
%=============================================================================
% Position set in pixels: the inner box drives the size.
widths = [60 100 130 150 180 200 230 260 300 350];
expected = [8 8 8 8.5 9 9 9.5 9.5 10 10];
for k = 1:numel(widths)
  f = figure('Visible', 'off', 'Position', [100 100 600 600]);
  pax = polaraxes(f, 'Units', 'pixels', 'Position', [50 50 widths(k) widths(k)]);
  drawnow();
  assert_isequal(pax.FontSize, expected(k));
  close(f);
end
% A low box shrinks whatever its width.
f = figure('Visible', 'off', 'Position', [100 100 600 600]);
pax = polaraxes(f, 'Units', 'pixels', 'Position', [50 50 400 80]);
drawnow();
assert_isequal(pax.FontSize, 8);
close(f);
%=============================================================================
% Small OuterPosition.
f = figure('Visible', 'off', 'Position', [100 100 400 300]);
pax = polaraxes(f, 'OuterPosition', [0 0 0.3 0.3]);
drawnow();
assert_isequal(pax.FontSize, 8);
% A user FontSize is kept.
pax.FontSize = 12;
drawnow();
assert_isequal(pax.FontSize, 12);
assert_isequal(char(pax.FontSizeMode), 'manual');
close(f);
%=============================================================================
