%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [status, message] = configuremingw(mingw_bin_path)
  status = false;
  message = '';
  clear('havecompiler');  % clear persistent variable
  if ~ispc()
    status = false;
    message = _('Not implemented on this platform.');
    return
  end
  if isdir(mingw_bin_path)
    arch = computer('arch');
    if strcmp(arch, 'win64')
      ref_gcc_compiler = [mingw_bin_path, '/', 'x86_64-w64-mingw32-gcc.exe'];
    else
      ref_gcc_compiler = [mingw_bin_path, '/', 'i686-w64-mingw32-gcc.exe'];
    end
    if isfile(ref_gcc_compiler)
      config.COMPILER_CHOICE = 'mingw';
      config.PATH = mingw_bin_path;
      json = jsonencode(config);
      json = jsonprettyprint(json);
      filewrite([prefdir(), '/compiler_', arch, '.json'], json);
      loadcompilerconf();
      status = true;
    else
      message = [ref_gcc_compiler, ' ', _('not found.')];
    end
  else
    message = [mingw_bin_path, ' ', _('not found.')];
  end
end
%=============================================================================