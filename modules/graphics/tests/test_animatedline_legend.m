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
f = figure('Visible', 'off', 'InnerPosition', [100 100 360 260], 'Color', [1 1 1]);
ax = axes('Parent', f);
%=============================================================================
legendLine = animatedline(ax, 1:3, [2 3 2], 'DisplayName', 'animated legend');
lgd = legend(ax);
asserts.istrue(isgraphics(lgd, 'legend'));
asserts.istrue(any(strcmp(lgd.String, 'animated legend')));
delete(legendLine);
%=============================================================================
