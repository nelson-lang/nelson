%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = vswhere()
  % https://github.com/Microsoft/vswhere
  if ~ispc()
    error(_('Not implemented on this platform.'));
  end
  arch = computer('arch');
  if (strcmp(arch, 'win64') == true || strcmp(arch, 'woa64') == true) 
    pathvswhere =  [getenv('ProgramFiles(x86)'), '\Microsoft Visual Studio\Installer\vswhere.exe'];
  else
    pathvswhere =  [getenv('ProgramFiles'), '\Microsoft Visual Studio\Installer\vswhere.exe'];
  end
  if ~isfile(pathvswhere)
    error(_('vswhere not found.'));
  end
  jsonfile = [tempdir(), 'vswhere.json'];
  cmd = ['"', pathvswhere, '"', ' -format json'];
  [status, message] = unix(cmd);
  status = (status == 0);
  if (status == false)
    error(_('Cannot read vswhere result.'));
  end
  r = jsondecode(message);
end
%=============================================================================
