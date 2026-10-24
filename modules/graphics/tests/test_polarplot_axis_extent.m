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
f = figure('Visible', 'off', 'InnerPosition', [100 100 560 420]);
ax = polaraxes('Parent', f);
polarplot(ax, linspace(0, pi, 50), linspace(0, 2, 50), 'o-');
rlim(ax, [0 2]);
thetalim(ax, [0 180]);
asserts.isequal(ax.XLim, [-2 2]);
asserts.isequal(ax.YLim, [0 2]);
frame = getframe(f);
img = double(frame.cdata);
figureBackground = squeeze(img(end - 10, 10, :))';
bottomCenter = squeeze(img(end - 10, round(size(img, 2) * 0.50), :))';
asserts.istrue(max(abs(bottomCenter - figureBackground)) < 8);
close(f);
%=============================================================================
