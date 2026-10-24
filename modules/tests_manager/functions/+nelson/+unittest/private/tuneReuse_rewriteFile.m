%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tuneReuse_rewriteFile(filename, enabled)
  pattern = '^[ \t]*%[ \t]*<--REUSE[ \t]+PROCESS-->[ \t]*$';
  replacement = '';
  if enabled
    replacement = '% <--REUSE PROCESS-->';
  end
  tuneHeaderTag_rewrite(filename, pattern, replacement); %#ok<NLS0004>
end
%=============================================================================
