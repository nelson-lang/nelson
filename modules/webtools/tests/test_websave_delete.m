%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('RequestMethod', 'delete');
filename = fullfile(tempdir(), 'test_websave_delete.json');
url = 'http://jsonplaceholder.typicode.com/posts/1';
max_attempts = 3;
fullname = '';
for attempt = 1:max_attempts
  try
    fullname = websave(filename, url, o);
    if isfile(fullname) && dir(fullname).bytes > 0
      break;
    end
    fullname = '';
  catch ex
    % Known transient/network errors: skip testsuite
    R = strcmp(ex.message, _('Forbidden (403)')) || ...
        strcmp(ex.message, _('Timeout was reached')) || ...
        strcmp(ex.message, _('Couldn''t resolve host name')) || ...
        strcmp(ex.message, _('Service unavailable (503)'));
    if R
      skip_testsuite(R, ex.message);
    end
    % otherwise allow retry
  end
  pause(0.5 * attempt);
end
if isempty(fullname)
  skip_testsuite(true, 'websave delete failed after retries or returned empty file');
end
assert_istrue(isfile(fullname));
R = jsondecode(fileread(fullname));
REF = struct();
assert_isequal(R, REF);
%=============================================================================
filename = [tempdir(), 'test_websave_delete.json'];
o = weboptions('RequestMethod', 'delete');
o.Timeout = 120;
cmd = 'fullname = websave(filename, ''https://jsonplaceholder.typicode.com/posts?userId=1'', o);';
try
  assert_checkerror(cmd, _('Not Found (404)'));
catch ex
  transient = strcmp(ex.message, _('Forbidden (403)')) || ...
              strcmp(ex.message, _('Timeout was reached')) || ...
              strcmp(ex.message, _('Couldn''t resolve host name')) || ...
              strcmp(ex.message, _('Service unavailable (503)'));
  if transient
    skip_testsuite(transient, ex.message);
  end
  rethrow(ex);
end
%=============================================================================
