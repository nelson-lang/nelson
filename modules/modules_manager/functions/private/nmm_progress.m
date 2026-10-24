%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_progress(action, name, version, step, index, total)
  % Progress hook for long package operations (install, update, remove).
  % nmm stays headless: the event is delivered only when a listener named
  % nmm_progress_listener is on the path (the package manager window
  % provides one); otherwise this is a no-op. Listener failures never
  % break the operation.
  if exist('nmm_progress_listener') == 0
    return
  end
  try
    nmm_progress_listener(action, name, version, step, index, total);
  catch
  end
end
%=============================================================================
