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
[T] = graphics_stackedplot_fixture();
s = stackedplot(T, {{'Temperature', 'Rain'}, 'Pressure'}, 'XVariable', 'Time');
asserts.isequal(length(s.Children), 2);
asserts.isequal(s.DisplayLabels, {'Temperature, Rain'; 'Pressure'});
asserts.isequal(length(s.Children(1).Children), 3);
asserts.isequal(s.AxesProperties(1).LegendLabels, {'Temperature', 'Rain'});
close all;
%=============================================================================
