%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Effective graphics backend for the run, selected by the 'Launcher' run option
% (transported through the NELSON_UNITTEST_LAUNCHER environment variable so it
% also reaches parallel worker processes). Values:
%   'default'  nelson-cli / -adv-cli / -gui; adv-cli tests run in its default
%              render mode (a batch run opens no figure window).
function backend = test_launcher_backend()
  backend = getenv('NELSON_UNITTEST_LAUNCHER');
  if isempty(backend)
    backend = 'default';
  end
  backend = lower(backend);
  if ~any(strcmp(backend, {'default'}))
    error('Nelson:tests_manager:unsupportedLauncherBackend', 'Unsupported test launcher backend: %s', backend);
  end
end
%=============================================================================
