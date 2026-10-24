%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Ask a concise question through a locally running model service.
options = aioptions('Provider', 'ollama', 'Model', 'llama3.2', 'Timeout', 20, 'NumPredict', 60);
try
  answer = aiask('Explain numerical conditioning in two sentences.', options);
catch exception
  error(['This example requires a local model service with llama3.2: ', exception.message]);
end
disp(answer);
