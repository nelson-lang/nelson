%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
[T] = graphics_stackedplot_fixture();
s = stackedplot(T, {'Temperature', 'Rain'});
axesProperties = s.AxesProperties;
close all;
axesProperties(1).LegendVisible = 'on';
axesProperties(1).LegendLabels = {'Temperature'};
s = stackedplot(T, {'Temperature', 'Rain'}, 'AxesProperties', axesProperties);
legendAxes = findobj(gcf(), 'Tag', 'legend');
asserts.istrue(~isempty(legendAxes));
close all;
%=============================================================================
