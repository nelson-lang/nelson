%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Inspect one response and save its referenced image in the temporary directory.
options = weboptions('Timeout', 15);
try
  info = webread('https://fakeface.rest/face/json', options);
  destination = fullfile(tempdir(), info.filename);
  resultFilename = websave(destination, info.image_url, options);
catch exception
  error(['This example requires network access to the public image service: ', exception.message]);
end
disp(['Downloaded image: ', resultFilename]);
