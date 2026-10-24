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
s = stackedplot(T, '--or', 'XVariable', 'Time');
asserts.isequal(s.Children(1).Children(end).LineStyle, '--');
asserts.isequal(s.Children(1).Children(end).Marker, 'o');
asserts.isequal(s.Children(1).Children(end).Color, [1 0 0]);
asserts.isequal(s.LineStyle, '--');
asserts.isequal(s.Marker, 'o');
close all;
%=============================================================================
