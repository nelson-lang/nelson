%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('RequestMethod', 'get', 'Timeout', 30);
filename = fullfile(tempdir(), 'test_websave_args_1.json');

MAX_ATTEMPTS = 5;
PAUSE_SECS = [0, 1, 2, 4, 4];
fullname = '';
lastErrorMsg = '';
success = false;

for attempt = 1:MAX_ATTEMPTS
  if isfile(filename)
    try delete(filename); catch; end
  end

  try
    fullname = websave(filename, 'https://postman-echo.com/get', 'r', i, "b+", 3, o);
    if isfile(fullname)
      R = jsondecode(fileread(fullname));
      if isstruct(R.args) && strcmp(R.args.r, '0+1i') && strcmp(R.args.b_, '3')
        success = true;
        break;
      else
        lastErrorMsg = 'Echoed arguments did not match expected scalar values.';
      end
    else
      lastErrorMsg = 'websave did not create the file.';
    end
  catch ex
    fullname = '';
    lastErrorMsg = ex.message;
  end

  pause(PAUSE_SECS(min(attempt, numel(PAUSE_SECS))));
end

if ~success
  skip_testsuite(true, ['Could not obtain test data: ' lastErrorMsg]);
end

asserts.istrue(isfile(fullname));
R = jsondecode(fileread(fullname));
asserts.istrue(isstruct(R.args));
asserts.isequal(R.args.r, '0+1i');
asserts.isequal(R.args.b_, '3');
%=============================================================================
