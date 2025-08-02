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
  if isfile(jsonfile)
    [status, msg] = rmfile(jsonfile);
  else
    status = true;
  end
end
%=============================================================================
