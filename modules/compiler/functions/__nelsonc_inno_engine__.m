%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function filename = __nelsonc_inno_engine__()
  filename = getenv('NELSONC_ISCC');
  if isempty(filename)
    filename = fullfile(getenv('ProgramFiles(x86)'), 'Inno Setup 6', 'ISCC.exe');
  end
  if ~ispc() || ~isfile(filename) || any(filename < char(32)) || contains(filename, '"')
    error('Nelson:nelsonc:installerTool:innoSetupRequired', 'Inno Setup 6 ISCC.exe is required. Set NELSONC_ISCC to its full path.');
  end
  filename = fullpath(filename);
end
