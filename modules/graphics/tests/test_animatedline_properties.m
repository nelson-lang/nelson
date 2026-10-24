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
an = animatedline(ax, 'Color', [1 0 0], 'LineStyle', '--', 'Marker', 'o', ...
  'MarkerFaceColor', [0 1 0], 'MarkerEdgeColor', [0 0 1], 'DisplayName', 'curve');
asserts.isequal(an.ColorMode, 'manual');
asserts.isequal(an.LineStyleMode, 'manual');
asserts.isequal(an.MarkerMode, 'manual');
asserts.isequal(an.Color, [1 0 0]);
asserts.isequal(an.LineStyle, '--');
asserts.isequal(an.Marker, 'o');
asserts.isequal(an.MarkerFaceColor, [0 1 0]);
asserts.isequal(an.MarkerEdgeColor, [0 0 1]);
an.Tag = 'animated-tag';
an.UserData = magic(2);
an.ButtonDownFcn = 'disp(1)';
an.CreateFcn = [];
an.DeleteFcn = [];
an.ContextMenu = gobjects(0, 0);
an.BusyAction = 'cancel';
an.Interruptible = 'off';
an.PickableParts = 'none';
an.Selected = 'on';
an.SelectionHighlight = 'off';
an.HitTest = 'off';
an.HandleVisibility = 'off';
asserts.isequal(an.Tag, 'animated-tag');
asserts.isequal(an.UserData, magic(2));
asserts.isequal(class(an.Annotation), 'nelson.graphics.eventdata.Annotation');
asserts.isequal(an.ButtonDownFcn, 'disp(1)');
asserts.isequal(an.CreateFcn, '');
asserts.isequal(an.DeleteFcn, '');
asserts.isequal(an.ContextMenu, gobjects(0, 0));
asserts.isequal(an.BusyAction, 'cancel');
asserts.isequal(char(an.Interruptible), 'off');
asserts.isequal(an.PickableParts, 'none');
asserts.isequal(char(an.Selected), 'on');
asserts.isequal(char(an.SelectionHighlight), 'off');
asserts.isequal(char(an.HitTest), 'off');
asserts.isequal(char(an.HandleVisibility), 'off');
asserts.isfalse(any(findobj(ax, 'Type', 'animatedline') == an));
an.HandleVisibility = 'on';
asserts.istrue(any(findobj(ax, 'Type', 'animatedline') == an));
%=============================================================================
