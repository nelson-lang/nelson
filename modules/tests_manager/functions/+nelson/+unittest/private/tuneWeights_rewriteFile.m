%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tuneWeights_rewriteFile(filename, weight)
  pattern = '^[ \t]*%[ \t]*<--WEIGHT[ \t]+[0-9]+-->[ \t]*$';
  replacement = '';
  if weight > 1
    replacement = ['% <--WEIGHT ', int2str(weight), '-->'];
  end
  tuneHeaderTag_rewrite(filename, pattern, replacement); %#ok<NLS0004>
end
%=============================================================================
