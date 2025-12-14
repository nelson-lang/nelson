%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mexext'), 1);
assert_isequal(nargout('mexext'), 1);
%=============================================================================
st = mexext('all');
assert_istrue(isstruct(st));
names = fieldnames(st);
assert_isequal(names, {'ext'; 'arch'});
assert_isequal(size(st), [8, 1]);
%=============================================================================
if strcmp(computer('arch') , 'win64')
  assert_isequal(mexext(), 'nexw64') 
end
%=============================================================================
if strcmp(computer('arch') , 'win32')
  assert_isequal(mexext(), 'nexw32') 
end
%=============================================================================
if strcmp(computer('arch') , 'woa64')
  assert_isequal(mexext(), 'nexwoa64') 
end
%=============================================================================
