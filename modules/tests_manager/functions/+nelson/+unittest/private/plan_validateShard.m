%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function plan_validateShard(shardIndex, shardCount)
  if shardCount < 1 || shardIndex < 1 || shardIndex > shardCount
    error('Nelson:tests_manager:invalidShard', 'Invalid shard configuration.');
  end
end
