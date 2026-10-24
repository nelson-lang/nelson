%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('prefdir'), 0);
asserts.isequal(nargout('prefdir'), 1);
%=============================================================================
pathPreferences = prefdir();
asserts.istrue(ischar(pathPreferences));
asserts.istrue(~isempty(pathPreferences));
asserts.istrue(isdir(pathPreferences));
%=============================================================================
