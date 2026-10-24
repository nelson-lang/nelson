%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cmd = append_process_option(cmd, enabled, option)
  if enabled
    cmd = [cmd, ' ', option];
  end
end
%=============================================================================
