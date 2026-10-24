%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_progress_listener(action, name, version, step, index, total)
  % Receives nmm progress events (see modules_manager nmm_progress) and
  % forwards them to the package manager window as one JSON payload.
  % The functions directory can outlive the gateway (removegateway keeps the
  % path): only notify while the nmm_gui module is loaded.
  if ~ismodule('nmm_gui')
    return
  end
  event = struct();
  event.action = action;
  event.name = name;
  event.version = version;
  event.step = step;
  event.index = index;
  event.total = total;
  __nmm_gui_notify__('progress', jsonencode(event));
end
%=============================================================================
