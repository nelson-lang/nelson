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
%=============================================================================
tic()
f = figure('Visible', 'off');
f.Color = [1, 1, 1];
colormap('turbo');

t = 0:(2 * pi) / 255:2 * pi;
hold on;
for k = 1:120
  radius = 1 + 0.003 * k;
  phase = k / 30;
  x = radius * cos(t + phase) + 0.02 * k;
  y = radius * sin(t + phase);
  fill(x, y, k / 120, 'EdgeColor', 'none');
end
hold off;
axis equal;
axis off;
drawnow();
toc()
close(f);
%=============================================================================
