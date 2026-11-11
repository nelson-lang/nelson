%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% webREST with a default weboptions must not send "Content-Type: auto".
o = weboptions();
o.Timeout = 60;
asserts.isequal(o.MediaType, 'auto');
filename = fullfile(tempdir(), 'test_webREST_media_type_auto.json');
url = 'https://postman-echo.com/headers';
max_attempts = 3;
fullname = '';
for attempt = 1:max_attempts
  try
    fullname = webREST(url, [], filename, struct(), o);
    if isfile(fullname) && dir(fullname).bytes > 0
      break;
    end
    fullname = '';
  catch ex
    transient = strcmp(ex.message, _('Forbidden (403)')) || ...
                strcmp(ex.message, _('Timeout was reached')) || ...
                strcmp(ex.message, _('Couldn''t resolve host name')) || ...
                strcmp(ex.message, _('Service unavailable (503)'));
    if transient
      skip_testsuite(transient, ex.message);
    end
  end
  pause(0.5 * attempt);
end
if isempty(fullname)
  skip_testsuite(true, 'webREST failed after retries or returned empty file');
end
R = jsondecode(fileread(fullname));
if isfile(fullname)
  rmfile(fullname);
end
asserts.istrue(isfield(R, 'headers'));
asserts.istrue(isfield(R.headers, 'content_type'));
asserts.isequal(R.headers.content_type, 'application/x-www-form-urlencoded');
%=============================================================================
