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
f = figure('Visible', 'off', 'Color', [1 1 1], 'Position', [100 100 240 240]);
ax = axes('Parent', f);
p = patch('Parent', ax, ...
  'Faces', [1 2 3 4], ...
  'Vertices', [0 0 0; 1 0 0; 1 1 0; 0 1 0], ...
  'FaceColor', 'none', 'EdgeColor', 'green', 'LineWidth', 2);
axis(ax, 'off');
axis(ax, [-0.1 1.1 -0.1 1.1 -1 1]);
view(ax, [0 0 1]);
drawnow();
F = getframe(ax);
centerPixel = double(squeeze(F.cdata(round(end / 2), round(end / 2), :)))';
asserts.istrue(all(centerPixel > 245));
asserts.isequal(p.FaceColor, 'none');
%=============================================================================
