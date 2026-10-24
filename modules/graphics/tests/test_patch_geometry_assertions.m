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
f = figure('Visible', 'off', 'Color', 'k', 'Position', [100 100 240 240]);
ax = axes('Parent', f);
axes(ax);
ax.Color = 'k';
p = patch([0 2 2 1 1 0], [0 0 2 2 1 1], 'y');
axis([0 2 0 2]);
axis('square');
axis('off');
drawnow();
F = getframe(ax);
h = size(F.cdata, 1);
w = size(F.cdata, 2);
xlim = ax.XLim;
ylim = ax.YLim;
missingCol = round((0.5 - xlim(1)) / (xlim(2) - xlim(1)) * (w - 1) + 1);
missingRow = round((ylim(2) - 1.5) / (ylim(2) - ylim(1)) * (h - 1) + 1);
filledCol = round((1.5 - xlim(1)) / (xlim(2) - xlim(1)) * (w - 1) + 1);
filledRow = round((ylim(2) - 0.5) / (ylim(2) - ylim(1)) * (h - 1) + 1);
missingPixel = double(squeeze(F.cdata(missingRow, missingCol, :)))';
filledPixel = double(squeeze(F.cdata(filledRow, filledCol, :)))';
asserts.istrue(all(missingPixel < 8));
asserts.istrue(filledPixel(1) > 245 && filledPixel(2) > 245 && filledPixel(3) < 8);
asserts.istrue(isgraphics(p, 'patch'));
