%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('nelsonroot'), 0);
asserts.isequal(nargout('nelsonroot'), 1);
%=============================================================================
rootPath = nelsonroot();
asserts.istrue(ischar(rootPath));
asserts.istrue(isdir(rootPath));
asserts.istrue(isfile([rootPath, '/README.md']));
%=============================================================================
