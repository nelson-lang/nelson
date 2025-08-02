%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('RequestMethod', 'put');
o.Timeout = 60;
filename = [tempdir(), 'test_websave_put.json'];
try
  fullname = websave(filename, 'https://jsonplaceholder.typicode.com/posts/1', o);
catch ex
  fullname = '';
  R = strcmp(ex.message, _('Forbidden (403)')) || ...
      strcmp(ex.message, _('Timeout was reached')) || ... 
      strcmp(ex.message, _('Couldn''t resolve host name'));
  skip_testsuite(R, ex.message)
end
R = jsondecode(fileread(fullname));
assert_isequal(R.id, 1);
%=============================================================================
