%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if strcmp(getenv('AUDIODEV'), 'null') == false && ~any(contains(argv(), '--no-audio'))
  % a lazy (never loaded) gateway cannot own live objects
  [~, ~, audioGatewayState] = gatewayinfo(modulepath('audio', 'builtin'));
  if strcmp(audioGatewayState, 'loaded')
    delete(audiorecorder_used());
    delete(audioplayer_used());
  end
  rmpath(modulepath('audio', 'functions'));
  removegateway(modulepath('audio', 'builtin'));
end
%=============================================================================
