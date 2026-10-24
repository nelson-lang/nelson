%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function run_impl_module_restore_environment(restoreEnvironment, previousPlatform)
  if restoreEnvironment.audio
    setenv('AUDIODEV', '');
  end
  if restoreEnvironment.platform
    setenv('QT_QPA_PLATFORM', previousPlatform);
  end
end
%=============================================================================
