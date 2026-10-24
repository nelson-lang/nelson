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
h = uicontrol();
R = properties(h);
REF = {'BackgroundColor';
    'BeingDeleted';
    'BusyAction';
    'ButtonDownFcn';
    'CData';
    'Callback';
    'Children';
    'ContextMenu';
    'CreateFcn';
    'DeleteFcn';
    'Enable';
    'Extent';
    'FontAngle';
    'FontName';
    'FontSize';
    'FontUnits';
    'FontWeight';
    'ForegroundColor';
    'HandleVisibility';
    'HorizontalAlignment';
    'InnerPosition';
    'Interruptible';
    'KeyPressFcn';
    'KeyReleaseFcn';
    'ListboxTop';
    'Max';
    'Min';
    'OuterPosition';
    'Parent';
    'Position';
    'SliderStep';
    'String';
    'Style';
    'Tag';
    'Tooltip';
    'Type';
    'Units';
    'UserData';
    'Value';
    'Visible'};
asserts.istrue(all(ismember(REF, R)));
%=============================================================================
close all
fig = figure('Visible', 'off', 'Position', [100 100 180 90], 'Color', [1 1 1]);
u = uicontrol('Parent', fig, 'Style', 'text', 'String', 'TEXT', ...
  'Position', [20 20 140 50], 'BackgroundColor', [1 1 1], ...
  'ForegroundColor', [1 0 0], 'FontSize', 24, 'FontWeight', 'bold');
if ~ismac()
  drawnow();
  frame = getframe(fig);
  img = double(frame.cdata) / 255;
  redPixels = img(:, :, 1) > 0.6 & img(:, :, 2) < 0.35 & img(:, :, 3) < 0.35;
  u.ForegroundColor = [0 0 1];
  drawnow();
  frame = getframe(fig);
  img = double(frame.cdata) / 255;
  bluePixels = img(:, :, 3) > 0.6 & img(:, :, 1) < 0.35 & img(:, :, 2) < 0.35;
  if nnz(redPixels) > 0 || nnz(bluePixels) > 0
    asserts.istrue(nnz(redPixels) > 0);
    asserts.istrue(nnz(bluePixels) > 0);
  end
end
close(fig);
%=============================================================================
