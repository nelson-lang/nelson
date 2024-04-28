%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempdir(), 'test_websave_get_3.json'];
try
  fullname = websave(filename, 'https://jsonplaceholder.typicode.com/posts/1/comments');
catch ex
  R = strcmp(ex.message, _('Forbidden (403)')) || ...
      strcmp(ex.message, _('Timeout was reached')) || ... 
      strcmp(ex.message, _('Couldn''t resolve host name'));
  skip_testsuite(R, ex.message)
end
R = jsondecode(fileread(fullname));
assert_isequal(R(5).email, 'Hayden@althea.biz');
%=============================================================================
