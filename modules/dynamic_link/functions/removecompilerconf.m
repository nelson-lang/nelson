%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function status = removecompilerconf()
  arch = computer('arch');
  clear('havecompiler'); % clear persistent variable
  jsonfile = [prefdir(), '/compiler_', arch, '.json'];
  status = false;
  for k = 1:20
    if ~isfile(jsonfile)
      pause(0.2);
      if ~isfile(jsonfile)
        status = true;
        clear('havecompiler');
        return
      end
    end
    status = rmfile(jsonfile);
    if ~isfile(jsonfile)
      pause(0.2);
      if ~isfile(jsonfile)
        status = true;
        clear('havecompiler');
        return
      end
    end
    pause(0.1);
  end
  status = ~isfile(jsonfile);
  clear('havecompiler');
end
%=============================================================================
