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
close all
f = figure('Visible', 'off');
xc = categorical({'small', 'large', 'small', 'medium'});
h = scatterhistogram(f, xc, [1 2 3 4]);
state = h.UserData;
asserts.isequal(state.XRenderData(:), double(xc(:)));
asserts.isequal(state.ScatterAxes.XTick, 1:numel(categories(xc)));
asserts.isequal(state.ScatterAxes.XTickLabel, cellstr(categories(xc)));
close(f);
close all
%=============================================================================
