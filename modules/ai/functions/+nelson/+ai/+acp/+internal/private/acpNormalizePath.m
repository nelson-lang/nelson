%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Normalize a filesystem path for comparing permission grants: trim it, unify the
% separator, and on Windows lowercase it (the file system is case-insensitive) so
% the same file matches whatever case or slash the agent used.
function out = acpNormalizePath(path)
  out = strtrim(char(path));
  if isempty(out)
    return
  end
  if ispc()
    out = lower(out);
    out = strrep(out, '\', '/');
  end
end
%=============================================================================
