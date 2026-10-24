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
close all
addpath([modulepath('graphics', 'tests')]);

f = figure('Visible', 'on', 'Position', [120 120 640 480]);
f.DrawLater = 'on';
ax = axes('Parent', f);
surf(peaks);
xlabel(ax, '');
ylabel(ax, '');
zlabel(ax, '');
title(ax, '');
axis(ax, 'vis3d');
view(ax, 3);
f.DrawLater = 'off';
drawnow();

for k = 1:4
  view(ax, -37.5 + k, 30 + 0.2 * k);
  drawnow();
end

asserts.isequal(numel(get(ax, 'Children')), 1);
frame = __getframe__(f, false);
render_assert_not_blank(frame);
close all
%=============================================================================
