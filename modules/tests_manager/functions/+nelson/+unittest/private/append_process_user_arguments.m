%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cmd = append_process_user_arguments(cmd, userArguments)
  if iscell(userArguments)
    quotedArguments = '';
    for k = 1:length(userArguments)
      quotedArguments = [quotedArguments, ' ', quoteProcessArgument(userArguments{k})];
    end
    userArguments = strtrim(quotedArguments);
  else
    userArguments = strtrim(userArguments);
  end
  if ~isempty(userArguments)
    cmd = [cmd, ' -- ', userArguments];
  end
end
%=============================================================================
