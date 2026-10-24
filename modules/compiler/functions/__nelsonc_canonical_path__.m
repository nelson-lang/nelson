%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function path = __nelsonc_canonical_path__(path)
  path = fullpath(path);
  if ~ismac()
    return;
  end
  quote = char(39);
  argument = [quote, strrep(path, quote, char([39 34 39 34 39])), quote];
  [status, resolved] = system(['/bin/realpath ', argument]);
  if status ~= 0
    error('Nelson:nelsonc:nativeMissing:cannotResolveRuntimeInput', 'Cannot resolve runtime input: %s.', path);
  end
  path = strtrim(resolved);
end
