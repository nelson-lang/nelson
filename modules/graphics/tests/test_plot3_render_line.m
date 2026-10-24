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
f = figure('Visible', 'off', 'Position', [100 100 360 260], 'Color', [0.9608 0.9608 0.9608]);
t = 0:pi / 50:10 * pi;
L = plot3(sin(t), cos(t), t);
axis square
grid on
delete(L)
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
