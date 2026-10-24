%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isdir'), 1);
asserts.isequal(nargout('isdir'), 1);
%=============================================================================
pathTest = tempname();
asserts.isfalse(isdir(pathTest));
mkdir(pathTest);
asserts.istrue(isdir(pathTest));
asserts.istrue(isdir(string(pathTest)));
rmdir(pathTest);
asserts.isfalse(isdir(pathTest));
%=============================================================================
