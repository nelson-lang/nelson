%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = bvpMergeOptions(options, extra)
  names = fieldnames(extra);
  for k = 1:length(names)
    name = bvpOptionName(names{k});
    options.(name) = extra.(names{k});
  end
end
%=============================================================================
