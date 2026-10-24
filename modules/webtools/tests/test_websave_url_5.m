%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('RequestMethod', 'get', 'ArrayFormat', 'php', 'Timeout', 30);
filename = fullfile(tempdir(), 'test_websave_args_5.json');
M = [1 2 3; 4 5 6];

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
    fullname = websave(filename, 'https://postman-echo.com/get', 'r', M, o);

    if isfile(fullname)
      R = jsondecode(fileread(fullname));
      if isstruct(R.args) && iscell(R.args.r) && length(R.args.r) == 6
        success = true;
        break;
      else
        lastErrorMsg = 'Echoed arguments did not match expected php array format.';
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
asserts.isequal(R.args.r{1}, '1');
asserts.isequal(R.args.r{2}, '4');
asserts.isequal(R.args.r{3}, '2');
asserts.isequal(R.args.r{4}, '5');
asserts.isequal(R.args.r{5}, '3');
asserts.isequal(R.args.r{6}, '6');
%=============================================================================
