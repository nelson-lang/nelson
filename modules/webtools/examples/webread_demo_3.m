%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Read a small structured response from a public API.
url = 'https://api.github.com/repos/nelson-lang/nelson';
options = weboptions('UserAgent', 'Nelson examples gallery', 'Timeout', 15);
try
  repository = webread(url, options);
catch exception
  error(['This example requires network access to the public API: ', exception.message]);
end
disp(['Repository: ', repository.full_name]);
disp(['Default branch: ', repository.default_branch]);
%=============================================================================
