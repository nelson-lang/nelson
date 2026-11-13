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
% Surface shares the image alpha code but stores AlphaData as a vector.
%=============================================================================
f = figure('Visible', 'off');
ax = axes(f);
h = surf(ax, peaks(3));
drawnow();
assert_isequal(h.AlphaData, 1);
h.AlphaData = 0.5 * ones(3);
h.FaceAlpha = 'flat';
drawnow();
assert_isequal(numel(h.AlphaData), 9);
h2 = __surf__('Parent', ax, 'XData', 1:3, 'YData', 1:3, 'ZData', magic(3));
drawnow();
assert_isequal(h2.AlphaData, 1);
close(f);
%=============================================================================
