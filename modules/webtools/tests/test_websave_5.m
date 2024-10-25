%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://jsonplaceholder.typicode.com/posts/1/comments';
filename = [tempdir(), 'test.txt'];
o = weboptions('ContentType','json');
o.Timeout = 60;
try
  destination_filename = websave(filename, url, o);
catch ex
  R = strcmp(ex.message, _('Forbidden (403)')) || ...
      strcmp(ex.message, _('Timeout was reached')) || ... 
      strcmp(ex.message, _('Couldn''t resolve host name'));
  skip_testsuite(R, ex.message)
end
assert_istrue(isfile(destination_filename));
txt = fileread(destination_filename);
st = jsondecode(txt);
assert_isequal(st(1).email, 'Eliseo@gardner.biz');
%=============================================================================
