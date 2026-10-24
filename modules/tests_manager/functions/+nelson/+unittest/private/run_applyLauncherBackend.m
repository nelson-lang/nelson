%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Publish the run's 'Launcher' selection through NELSON_UNITTEST_LAUNCHER so the
% launcher builders (and any spawned worker process) pick the right graphics
% backend, and restore the previous value when the returned cleanup expires.
function cleanup = run_applyLauncherBackend(launcher)
  if isempty(launcher)
    launcher = 'default';
  end
  launcher = lower(launcher);
  if ~any(strcmp(launcher, {'default', 'webview'}))
    error('Nelson:tests_manager:unsupportedLauncher', 'Unsupported ''Launcher'' value: %s (use ''default'' or ''webview'').', launcher);
  end
  previous = getenv('NELSON_UNITTEST_LAUNCHER');
  setenv('NELSON_UNITTEST_LAUNCHER', launcher);
  cleanup = onCleanup(@() setenv('NELSON_UNITTEST_LAUNCHER', previous));
end
%=============================================================================
