%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Effective graphics backend for the run, as selected by the 'Launcher' run
% option (transported through the NELSON_UNITTEST_LAUNCHER environment variable
% so it also reaches parallel worker processes). Values:
%   'default'  the historical executables (nelson-cli / -adv-cli / -gui)
%   'webview'  ADV-CLI-tagged tests run with `nelson-adv-cli --webview`, i.e.
%              figures rendered by the headless RenderWeb backend; cli/gui tags
%              are unchanged (routing gui tests to the web desktop needs the
%              browser I/O plumbing and is a separate effort).
function backend = test_launcher_backend()
  backend = getenv('NELSON_UNITTEST_LAUNCHER');
  if isempty(backend)
    backend = 'default';
  end
  backend = lower(backend);
  if ~any(strcmp(backend, {'default', 'webview'}))
    error('Nelson:tests_manager:unsupportedLauncherBackend', 'Unsupported test launcher backend: %s', backend);
  end
end
%=============================================================================
