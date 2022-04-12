%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function edit(varargin)
  nargin = length(varargin);
  if nargin == 0
    editor();
  else
    for v = varargin(:)'
      name = v{1};
      if ismacro(name)
        filename = which(name);
        if iscell(filename)
          finalfilename = filename{1};
        else
          finalfilename = filename;
        end
        editor(finalfilename);
      else
        finalfilename =name;
      end
      if isdir(finalfilename)
        msg = [_('Cannot edit the directory: '), finalfilename];
        error(msg)
      end
      if strcmp(fileparts(finalfilename, 'extension'), '') == true
        finalfilename = [finalfilename, '.m'];
      end
      editor(finalfilename)
    end
  end
end
%=============================================================================
