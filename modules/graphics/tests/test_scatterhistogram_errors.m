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
[x, y] = graphics_scatterhistogram_fixture();
f = figure('Visible', 'off');
ax = axes();
asserts.checkerror('scatterhistogram(ax, x, y);', getString(message('Nelson:scatterhistogram:scatterHistogramChartCannotBeA')));
close(f);
%=============================================================================
asserts.checkerror('scatterhistogram(1);', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('scatterhistogram([1 2], [1 2 3]);', getString(message('Nelson:dimagree:valueValueNumberElements', 'X', 'Y')));
asserts.checkerror('scatterhistogram([1 2], [1 2], ''NumBins'', 0);', getString(message('Nelson:scatterhistogram:numBinsScalarVectorMatrix')));
asserts.checkerror('scatterhistogram([1 2], [1 2], ''NumBins'', [2 3 4]);', getString(message('Nelson:graphics:invalidNumBins')));
asserts.checkerror('scatterhistogram([1 2], [1 2], ''HistogramDisplayStyle'', ''area'');', getString(message('Nelson:scatterhistogram:invalidHistogramDisplayStyle')));
%=============================================================================
close all
%=============================================================================
