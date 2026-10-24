%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('websave'), -1);
asserts.isequal(nargout('websave'), 1);
%=============================================================================
url = 'https://jsonplaceholder.typicode.com/posts/1';
filename = fullfile(tempdir(), 'test_websave_1.json');
options = weboptions('Timeout', 30);
max_attempts = 3;
destination_filename = '';
for attempt = 1:max_attempts
  if isfile(filename)
    try delete(filename); catch; end
  end
  try
    destination_filename = websave(filename, url, options);
    if isfile(destination_filename) && dir(destination_filename).bytes > 0
      break;
    end
    destination_filename = '';
  catch ex
    R = strcmp(ex.message, _('Forbidden (403)')) || ...
        strcmp(ex.message, _('Timeout was reached')) || ...
        strcmp(ex.message, _('Couldn''t resolve host name')) || ...
        strcmp(ex.message, _('Service unavailable (503)'));
    if R
      skip_testsuite(R, ex.message);
    end
  end
  pause(0.5 * attempt);
end
if isempty(destination_filename)
  skip_testsuite(true, 'websave failed after retries or returned empty file');
end
asserts.istrue(isfile(destination_filename));
R = jsondecode(fileread(destination_filename));
asserts.isequal(R.id, 1);
asserts.isequal(R.userId, 1);
asserts.isequal(R.title, 'sunt aut facere repellat provident occaecati excepturi optio reprehenderit');
%=============================================================================
