%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
f = figure('Color', [1 1 1]);
[x, y, z] = peaks(40);
surf(x, y, z, 'EdgeColor', 'none', 'FaceLighting', 'gouraud');
light('Position', [1 -1 1], 'Style', 'infinite');
material('shiny');
colormap(parula(64));
view(35, 28);
title('Lit surface');
%=============================================================================
