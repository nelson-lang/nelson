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
f = figure('Visible', 'off', 'Position', [100 100 620 460]);
ax1 = subplot(1, 2, 1, 'Parent', f);
surf(ax1, peaks(12));
view(ax1, -37.5, 30);
ax2 = subplot(1, 2, 2, 'Parent', f);
surf(ax2, peaks(12));
view(ax2, 10, 20);
drawnow();

jsondecode(__web_display_list__(f, ax2, 45, 35, false));

asserts.isequal(ax1.View, [-37.5 30]);
asserts.isequal(ax2.View, [45 35]);

close(f);
%=============================================================================
