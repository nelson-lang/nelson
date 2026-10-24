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
%=============================================================================
f = figure('Visible', 'off');
s = stackedplot(f, T, {'Temperature', 'Rain'});
asserts.isequal(s.Parent, f);
asserts.isequal(s.Children(1).Parent, f);
close all;
%=============================================================================
f = figure('Visible', 'off', 'DrawLater', 'on');
s = stackedplot(f, T, {'Temperature', 'Rain'});
asserts.isequal(s.Parent, f);
asserts.isequal(char(f.DrawLater), 'on');
asserts.isequal(length(s.Children), 2);
asserts.isequal(s.Children(1).Children(end).Type, 'line');
asserts.isequal(s.Children(1).Children(end - 1).String, 'Temperature');
s.LineWidth = 1.25;
s.GridVisible = 'on';
asserts.isequal(s.Children(1).Children(end).LineWidth, 1.25);
asserts.isequal(char(s.Children(1).XGrid), 'on');
f.DrawLater = 'off';
asserts.isequal(char(f.DrawLater), 'off');
asserts.isequal(s.Children(1).Children(end).LineWidth, 1.25);
asserts.isequal(char(s.Children(1).XGrid), 'on');
close all;
%=============================================================================
