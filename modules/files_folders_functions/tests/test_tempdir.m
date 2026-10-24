%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('tempdir'), 0);
asserts.isequal(nargout('tempdir'), 1);
%=============================================================================
pathTemp = tempdir();
asserts.istrue(ischar(pathTemp));
asserts.istrue(isdir(pathTemp));
asserts.istrue(pathTemp(end) == '/' || pathTemp(end) == '\');
filename = [pathTemp, 'nelson_tempdir_test_', createGUID(), '.txt'];
filewrite(filename, 'Nelson');
asserts.istrue(isfile(filename));
rmfile(filename);
%=============================================================================
