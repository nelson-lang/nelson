%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a lazy (never loaded) gateway has no profiling session to stop
[~, ~, profilerGatewayState] = gatewayinfo(modulepath('profiler', 'builtin'));
if strcmp(profilerGatewayState, 'loaded')
  profile('off')
  profile('clear')
end
%=============================================================================
removegateway(modulepath('profiler', 'builtin'))
%=============================================================================
