%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_editor_layout__(window)
  state = window.UserData;
  if ~isstruct(state) || ~isfield(state, 'Tabs'); return; end
  position = window.Position;
  width = max(position(3), 800);
  height = max(position(4), 680);
  if width ~= position(3) || height ~= position(4)
    window.Position = [position(1:2), width, height];
  end
  top = height - 46;
  for k = 1:4; state.Buttons{k}.Position = [12 + (k - 1) * 44, top, 36, 34]; end
  left = width - 12 - 120 - 100 - 108 - 16;
  state.ProjectLabel.Position = [196, top, max(1, left - 208), 34];
  sizes = [108, 100, 120];
  for k = 1:3
    state.Buttons{k + 4}.Position = [left, top, sizes(k), 34];
    left = left + sizes(k) + 8;
  end
  state.Tabs.Position = [12, 170, width - 24, height - 228];
  state.Log.Position = [12, 38, width - 24, 120];
  state.Status.Position = [12, 8, width - 24, 24];
  state.Table.ColumnWidth = {floor((width - 92) * 0.68), floor((width - 92) * 0.32)};
  paneWidth = (width - 60) / 2;
  paneHeight = height - 274;
  for k = 1:numel(state.Forms); state.Forms{k}.Position = [0, 0, width - 28, height - 254]; end
  for k = 1:2
    area = state.Resources{k};
    area.Panel.Position = [12 + (k - 1) * (paneWidth + 12), 8, paneWidth, paneHeight];
    area.Label.Position = [0, paneHeight - 26, paneWidth, 24];
    area.List.Position = [0, 44, paneWidth, max(1, paneHeight - 80)];
    buttonWidth = (paneWidth - 16) / 3;
    for j = 1:3; area.Buttons{j}.Position = [(j - 1) * (buttonWidth + 8), 4, buttonWidth, 30]; end
  end
end
