%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://jsonplaceholder.typicode.com/todos/1';
filename = fullfile(tempdir(), 'test_websave_6.json');
o = weboptions();
o.Timeout = 30;

MAX_ATTEMPTS = 3;
PAUSE_SECS = [0, 1, 2];
destination_filename = '';
lastErrorMsg = '';
success = false;

for attempt = 1:MAX_ATTEMPTS
  % remove any stale file before trying
  if isfile(filename)
    try delete(filename); catch; end
  end

  try
    destination_filename = websave(filename, url, o);

    if isfile(destination_filename)
      R = jsondecode(fileread(destination_filename));
      if isstruct(R) && R.id == 1
        success = true;
        break;
      else
        lastErrorMsg = 'Downloaded content did not contain expected todo.';
      end
    else
      lastErrorMsg = 'websave did not create the file.';
    end
  catch ex
    lastErrorMsg = ex.message;
  end

  pause(PAUSE_SECS(min(attempt, numel(PAUSE_SECS))));
end

if ~success
  skip_testsuite(true, ['Could not obtain test data: ' lastErrorMsg]);
end

asserts.istrue(isfile(destination_filename));
R = jsondecode(fileread(destination_filename));
asserts.isequal(R.id, 1);
asserts.isequal(R.userId, 1);
asserts.isequal(R.completed, false);
%=============================================================================
