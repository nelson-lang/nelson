%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
asserts.isequal(nargin('mexext'), 1);
asserts.isequal(nargout('mexext'), 1);
%=============================================================================
st = mexext('all');
asserts.istrue(isstruct(st));
names = fieldnames(st);
asserts.isequal(names, {'ext'; 'arch'});
asserts.isequal(size(st), [8, 1]);
idx = strcmp({st.arch}, 'maca64');
asserts.isequal(sum(idx), 1);
asserts.isequal(st(idx).ext, 'nexmaca64');
%=============================================================================
if strcmp(computer('arch'), 'win64')
  asserts.isequal(mexext(), 'nexw64')
end
%=============================================================================
if strcmp(computer('arch'), 'win32')
  asserts.isequal(mexext(), 'nexw32')
end
%=============================================================================
if strcmp(computer('arch'), 'woa64')
  asserts.isequal(mexext(), 'nexwoa64')
end
%=============================================================================
if strcmp(computer('arch'), 'maca64')
  asserts.isequal(mexext(), 'nexmaca64')
end
%=============================================================================
