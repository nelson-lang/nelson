%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Prepare an authenticated request payload without sending it.
apiKey = getenv('CHATGPT_API_KEY', '');
if isempty(apiKey)
  error('This example requires CHATGPT_API_KEY to be defined.');
end
payload.model = 'configured-model';
payload.prompt = 'Explain numerical conditioning in two sentences.';
payload.max_tokens = 120;
payload.temperature = 0.2;
disp('Request payload prepared. No network request is sent by this example.');
disp(jsonprettyprint(jsonencode(payload)));
%=============================================================================
