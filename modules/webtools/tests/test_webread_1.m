%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('webread'), -1);
assert_isequal(nargout('webread'), -1);
%=============================================================================
o = weboptions('RequestMethod', 'get');
o.Timeout = 50;
try
  R = webread('https://jsonplaceholder.typicode.com/posts/1/comments', o);
catch ex
  R = strcmp(ex.message, _('Forbidden (403)')) || ...
      strcmp(ex.message, _('Timeout was reached')) || ... 
      strcmp(ex.message, _('Couldn''t resolve host name'));
  skip_testsuite(R, ex.message)
end
assert_istrue(isstruct(R));
assert_isequal(R(1).email, 'Eliseo@gardner.biz');
%=============================================================================
