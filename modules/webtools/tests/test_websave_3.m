%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://s.w-x.co/staticmaps/WEB_Current_Weather_Map_1280x720.jpg?crop=16:9&width=800&format=pjpg&auto=webp&quality=60';
filename = [tempdir(), 'earth2.jpg'];
if isfile(filename)
  rmfile(filename);
end
testPass = false;
i = 0;
retry = true;
while (retry)
  try
    destination_filename = websave(filename, url);
    testPass = true;
  catch ex
    destination_filename = '';
    testPass = (strcmp(ex.message, 'Bad Request (400)') == 1);
    if ~testPass
      testPass = (strcmp(ex.message, _('Timeout was reached')) == 1);
    end
  end
  i = i + 1;
  retry = ~testPass && (i < 5);
end
if isvar('ex')
  R = strcmp(ex.message, _('Forbidden (403)')) || ...
  strcmp(ex.message, _('Bad Request (400)')) || ... 
  strcmp(ex.message, _('Timeout was reached')) || ... 
  strcmp(ex.message, _('Couldn''t resolve host name'));
  skip_testsuite(R, ex.message)
end
assert_istrue(isfile(destination_filename));
assert_istrue(testPass)
%=============================================================================
