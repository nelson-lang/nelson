%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% Legend and colorbar positions in pixel units are logical, 1-based pixels:
% the same place given in normalized or in pixel units paints the same frame,
% whatever the display scale of a shown figure.
%=============================================================================
close all
%=============================================================================
for visible = {'off', 'on'}
  f = figure('Visible', visible{1}, 'Position', [100 100 400 300], 'Color', [1 1 1]);
  ax = axes(f, 'Position', [0.1 0.1 0.5 0.8]);
  plot(ax, 1:10, 'r');
  lg = legend(ax, 'data');
  cb = colorbar(ax);
  drawnow();
  lg.Units = 'normalized';
  lg.Position = [0.65 0.7 0.3 0.1];
  cb.Units = 'normalized';
  cb.Position = [0.8 0.1 0.05 0.5];
  drawnow();
  normalizedFrame = getframe(f).cdata;
  lg.Units = 'pixels';
  cb.Units = 'pixels';
  fw = f.Position(3);
  fh = f.Position(4);
  asserts.isapprox(lg.Position, [0.65 * fw + 1, 0.7 * fh + 1, 0.3 * fw, 0.1 * fh], 1e-6);
  asserts.isapprox(cb.Position, [0.8 * fw + 1, 0.1 * fh + 1, 0.05 * fw, 0.5 * fh], 1e-6);
  drawnow();
  pixelFrame = getframe(f).cdata;
  changedPixels = nnz(any(pixelFrame ~= normalizedFrame, 3));
  asserts.istrue(changedPixels <= 0.03 * size(normalizedFrame, 1) * size(normalizedFrame, 2));
  close(f);
end
%=============================================================================
% A legend placed in a tiledlayout tile keeps its logical size.
tileWidth = zeros(1, 2);
visibility = {'off', 'on'};
for k = 1:2
  f = figure('Visible', visibility{k}, 'Position', [100 100 400 300]);
  t = tiledlayout(f, 1, 1);
  ax = nexttile(t);
  plot(ax, 1:10);
  lg = legend(ax, 'data');
  lg.Layout.Tile = 'east';
  drawnow();
  tileWidth(k) = lg.Position(3) * f.Position(3);
  close(f);
end
asserts.isapprox(tileWidth(2), tileWidth(1), 1e-6);
%=============================================================================
