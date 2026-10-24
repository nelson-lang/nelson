%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function files = getFilesToTest(param) %#ok<NLS0026>
  files = [];
  if iscellstr(param)
    for k = param(:)'
      if isfile(k{1})
        resolved = fullpath(k{1});
        if isempty(files)
          files = {resolved};
        else
          files = [files; resolved];
        end
      end
    end
  elseif ischar(param)
    if isfile(param)
      files = {fullpath(param)};
    end
  end
end
