%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% An image whose XData or YData is given the other way round (descending) flips
% the picture along that axis. movie() relies on a descending YData to put row 1
% of every frame at the top. The web display list used to clip such a span to a
% point and emit no image at all, leaving the figure blank.
%=============================================================================
C = uint8(randi(255, [100, 120, 3]));
%=============================================================================
% Ascending spans: the baseline that always worked.
f = figure('Visible', 'off');
image('CData', C, 'XData', [1, 120], 'YData', [1, 100]);
ax = gca(); ax.XLim = [0.5, 120.5]; ax.YLim = [0.5, 100.5];
asserts.isequal(count(__web_display_list__(f), '"kind":"image"'), 1);
close(f);
%=============================================================================
% Descending YData (movie() frame): the regression case, zero images before.
f = figure('Visible', 'off');
image('CData', C, 'XData', [0, 1], 'YData', [1, 0]);
ax = gca(); ax.XLim = [0, 1]; ax.YLim = [0, 1];
asserts.isequal(count(__web_display_list__(f), '"kind":"image"'), 1);
close(f);
%=============================================================================
% Descending XData mirrors horizontally and must still be drawn.
f = figure('Visible', 'off');
image('CData', C, 'XData', [1, 0], 'YData', [0, 1]);
ax = gca(); ax.XLim = [0, 1]; ax.YLim = [0, 1];
asserts.isequal(count(__web_display_list__(f), '"kind":"image"'), 1);
close(f);
%=============================================================================
% The movie() helper drives the reversed YData path end to end.
frames(2) = struct('cdata', [], 'colormap', []);
frames(1).cdata = C;
frames(2).cdata = uint8(randi(255, [100, 120, 3]));
f = figure('Visible', 'off');
movie(frames, 1, 24);
asserts.isequal(count(__web_display_list__(f), '"kind":"image"'), 1);
close(f);
%=============================================================================
