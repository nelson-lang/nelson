%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
try
  error('MyPkg:boom', 'exploded %d', 7);
catch
end
%=============================================================================
[m, id] = lasterr();
assert_isequal(id, 'MyPkg:boom');
assert_istrue(ischar(m));
assert_istrue(~isempty(strfind(m, 'exploded 7')));
%=============================================================================
prev = lasterr('new message', 'New:id');
assert_istrue(~isempty(strfind(prev, 'exploded 7')));
[m2, id2] = lasterr();
assert_isequal(m2, 'new message');
assert_isequal(id2, 'New:id');
%=============================================================================
lasterr('only message');
[m3, id3] = lasterr();
assert_isequal(m3, 'only message');
assert_isequal(id3, '');
%=============================================================================
lasterr('with string arg');
assert_isequal(lasterr(), 'with string arg');
%=============================================================================
