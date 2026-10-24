%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Send a fixed prompt to a locally running model service.
exampleDirectory = fileparts(mfilename('fullpathext'));
previousPath = addpath(exampleDirectory);
cleanupPath = onCleanup(@() path(previousPath));
try
  answer = ollamaPrompt('Explain numerical conditioning in two sentences.');
catch exception
  error(['This example requires a local model service with the llama3 model: ', exception.message]);
end
disp(answer);
clear cleanupPath;
