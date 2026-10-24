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
figure();
x = linspace(-2 * pi, 2 * pi);
y = linspace(0, 4 * pi);
[X, Y] = meshgrid(x, y);
Z = sin(X) + cos(Y);
contour3(X, Y, Z);
%=============================================================================
[M, h] = contour3(X, Y, Z);
R = properties(h);
REF = {'BeingDeleted';
  'BusyAction';
  'Children';
  'ContourMatrix';
  'CreateFcn';
  'DeleteFcn';
  'DisplayName';
  'EdgeAlpha';
  'EdgeColor';
  'FaceAlpha';
  'FaceColor';
  'Interruptible';
  'LabelColor';
  'LabelFormat';
  'LabelSpacing';
  'LevelList';
  'LevelListMode';
  'LevelStep';
  'LevelStepMode';
  'LineStyle';
  'LineWidth';
  'Parent';
  'ShowText';
  'Tag';
  'TextList';
  'TextListMode';
  'TextStep';
  'TextStepMode';
  'Type';
  'UserData';
  'Visible';
  'XData';
  'XDataMode';
  'YData';
  'YDataMode';
  'ZData';
  'ZLocation'};
asserts.istrue(all(ismember(REF, R)));
asserts.isequal(M, h.ContourMatrix);
asserts.isequal(char(h.Floating), 'on');
asserts.isequal(h.FaceColor, 'none');
asserts.isequal(char(h.ShowText), 'off');
%=============================================================================
