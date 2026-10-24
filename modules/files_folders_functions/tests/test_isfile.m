%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isfile'), 1);
asserts.isequal(nargout('isfile'), 1);
%=============================================================================
filename = [tempname(), '.txt'];
asserts.isfalse(isfile(filename));
filewrite(filename, 'Nelson');
asserts.istrue(isfile(filename));
asserts.istrue(isfile(string(filename)));
rmfile(filename);
asserts.isfalse(isfile(filename));
%=============================================================================
