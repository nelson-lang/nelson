%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function pattern = discover_filenamePattern(filename)
  [~, name, ~] = fileparts(filename); %#ok<NLS0004>
  if startsWith(name, 'test_')
    pattern = 'test_*.m';
  elseif startsWith(name, 'bug_')
    pattern = 'bug_*.m';
  elseif startsWith(name, 'bench_')
    pattern = 'bench_*.m';
  else
    pattern = '';
  end
end
