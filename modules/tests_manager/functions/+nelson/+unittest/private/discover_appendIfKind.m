%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function files = discover_appendIfKind(files, filename, kind)
  if any(strcmp(discover_patternsForKind(kind), discover_filenamePattern(filename)))
    files = [files; filename];
  end
end
