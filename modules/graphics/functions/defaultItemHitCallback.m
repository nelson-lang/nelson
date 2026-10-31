%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Default legend item hit callback: a double-click on a label starts text editing.
function defaultItemHitCallback(src, evt)
  if strcmp(evt.SelectionType, 'open') && strcmp(evt.Region, 'label')
    startLabelEditing(src, evt.Peer);
  end
end
%=============================================================================
function startLabelEditing(lg, peer)
  figureHandle = ancestor(lg, 'figure');
  if isempty(figureHandle)
    return
  end
  labels = lg.String;
  index = find(strcmp(labels, peer.DisplayName), 1);
  if isempty(index)
    return
  end
  delete(findall(figureHandle, 'Tag', 'LegendLabelEditor'));
  box = labelEditorPosition(lg, figureHandle, index);
  editor = uicontrol('Parent', figureHandle, 'Style', 'edit', 'Units', 'pixels', ...
    'Position', box, 'String', labels{index}, 'FontSize', lg.FontSize, ...
    'HorizontalAlignment', 'left', 'Tag', 'LegendLabelEditor', ...
    'Callback', @(h, ~) commitLabel(h, lg, index));
  uicontrol(editor);
end
%=============================================================================
function box = labelEditorPosition(lg, figureHandle, index)
  units = lg.Units;
  lg.Units = 'normalized';
  position = lg.Position;
  lg.Units = units;
  figurePosition = figureHandle.Position;
  width = figurePosition(3);
  height = figurePosition(4);
  rowHeight = max(10, ceil(lg.FontSize * 1.35));
  tokenSize = lg.ItemTokenSize;
  left = position(1) * width + 6 + tokenSize(1) + 4;
  top = (position(2) + position(4)) * height - 4 - rowHeight * (index - 1);
  box = [left, top - rowHeight, max(20, position(3) * width - (left - position(1) * width) - 4), ...
    rowHeight];
end
%=============================================================================
function commitLabel(editor, lg, index)
  text = editor.String;
  if iscell(text)
    text = text{1};
  end
  if isgraphics(lg) && index <= numel(lg.String)
    labels = lg.String;
    labels{index} = text;
    lg.String = labels;
  end
  delete(editor);
end
%=============================================================================
