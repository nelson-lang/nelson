%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = dlgetnelsonincludes()
  if isdir([modulepath('interpreter'), '/src/include'])
    c = {[modulepath('interpreter'), '/src/include']; ...
    [modulepath('types'), '/src/include']; ...
    [modulepath('error_manager'), '/src/include']; ...
    [modulepath('i18n'), '/src/include']; ...
    [modulepath('stream_manager'), '/src/include']};
    
    if (ismodule('f2c'))
      c = [c; [modulepath('f2c'), '/src/include']];
    end
    if (ismodule('validators'))
      c = [c; [modulepath('validators'), '/src/include']];
    end
  else
    if (isdir([modulepath('nelson', 'builtin'), '/../../include/Nelson/interpreter']))
      includeRootPath = fullpath([modulepath('nelson', 'builtin'), '/../../include/Nelson/']);
      c = {[includeRootPath, '/interpreter']; ...
      [includeRootPath, '/types']; ...
      [includeRootPath, '/error_manager']; ...
      [includeRootPath, '/i18n']; ...
      [includeRootPath, '/stream_manager']};
      
      if (ismodule('f2c'))
        c = [c; [includeRootPath, '/f2c']];
      end
      if (ismodule('validators'))
        c = [c; [includeRootPath, '/validators']];
      end
      
    end
  end
end
%=============================================================================
