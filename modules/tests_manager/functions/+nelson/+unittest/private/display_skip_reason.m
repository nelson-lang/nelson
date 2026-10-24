%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function display_skip_reason(testCase)
  if testCase.skip && ~isempty(testCase.msg)
    msg = sprintf('    Reason: %s\n', testCase.msg);
    if length(msg) > 75
      msg = [msg(1:75), '...', '\n'];
    end
    fprintf(stdout, msg);
  end
end
%=============================================================================
