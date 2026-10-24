%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function polarAppendDataHandles(ax, handles)
  state = polarGetState(ax, false);
  state.DataHandles = [state.DataHandles; handles(:)];
  polarSetState(ax, state);
  polarRefresh(ax, state, false);
end
%=============================================================================
