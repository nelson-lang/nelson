%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('pwd'), 0);
asserts.isequal(nargout('pwd'), 1);
%=============================================================================
current = pwd();
asserts.istrue(ischar(current));
asserts.istrue(isdir(current));
pathTest = tempname();
mkdir(pathTest);
cd(pathTest);
computed = pwd();
expected = fullpath(pathTest);
if ismac() && startsWith(computed, '/private/') && strcmp(computed(9:end), expected)
  computed = expected;
end
asserts.isequal(computed, expected);
cd(current);
rmdir(pathTest);
%=============================================================================
