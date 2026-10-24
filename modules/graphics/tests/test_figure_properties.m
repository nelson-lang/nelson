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
f = figure();
asserts.isequal(class(f), 'nelson.ui.Figure');
asserts.istrue(isobject(f));
asserts.istrue(isa(f, 'nelson.ui.Figure'));
asserts.istrue(isa(f, 'handle'));
asserts.istrue(isa(f, 'graphics_object'));
asserts.isequal(f.Color, [0.94 0.94 0.94]);
asserts.istrue(f.AutoResizeChildren == 'off');
asserts.isequal(f.NextPlot, 'add');
p = properties(f);
REF = {'Alphamap';
  'BeingDeleted';
  'BusyAction';
  'ButtonDownFcn';
  'Children';
  'CloseRequestFcn';
  'Color';
  'Colormap';
  'CreateFcn';
  'CurrentAxes';
  'CurrentObject';
  'DeleteFcn';
  'GraphicsSmoothing';
  'HandleVisibility';
  'Interruptible';
  'KeyPressFcn';
  'KeyReleaseFcn';
  'MenuBar';
  'Name';
  'NextPlot';
  'Number';
  'NumberTitle';
  'Parent';
  'Position';
  'Resize';
  'SelectionHighlight';
  'SizeChangedFcn';
  'Tag';
  'ToolBar';
  'Type';
  'UserData';
  'Visible';
  'WindowState'};
asserts.istrue(all(ismember(REF, p)));
%=============================================================================
f.Pointer = 'crosshair';
drawnow();
asserts.isequal(f.Pointer, 'crosshair');
f.PointerShapeCData = [1 NaN; 2 1];
f.PointerShapeHotSpot = [2 1];
f.Pointer = 'custom';
drawnow();
asserts.isequal(f.Pointer, 'custom');
asserts.istrue(isequaln(f.PointerShapeCData, [1 NaN; 2 1]));
asserts.isequal(f.PointerShapeHotSpot, [2 1]);
%=============================================================================
%=============================================================================
function position = expected_default_figure_position()
  position = groot().DefaultFigurePosition;
end
%=============================================================================
function frameSize = expected_figure_frame_size(f)
  position = f.Position;
  % A hidden empty figure keeps logical pixels; visible figures and figures
  % containing axes are captured at device resolution.
  if strcmp(f.Visible, 'off') && isempty(findobj(f, 'Type', 'axes'))
    frameSize = [round(position(4)), round(position(3)), 3];
  else
    ratio = f.DevicePixelRatio;
    frameSize = [round(position(4) * ratio), round(position(3) * ratio), 3];
  end
end
%=============================================================================
function visibleSize = expected_visible_figure_size(position, f)
  visibleSize = position(3:4) / f.DevicePixelRatio;
end
%=============================================================================
