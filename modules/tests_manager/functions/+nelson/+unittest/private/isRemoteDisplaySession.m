%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = isRemoteDisplaySession()
  if isTruthyEnvironmentValue('NELSON_REMOTE_DISPLAY_SESSION')
    res = true;
    return
  end
  names = {'CHROME_REMOTE_DESKTOP_SESSION', 'CHROME_REMOTE_DESKTOP', ...
    'CRD_SESSION', 'VNC_SESSION', 'NXSESSIONID'};
  for k = 1:length(names)
    if isTruthyEnvironmentValue(names{k})
      res = true;
      return
    end
  end
  res = false;
end
%=============================================================================
