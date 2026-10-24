%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('license'), -1);
asserts.isequal(nargout('license'), -1);
%=============================================================================
license();
%=============================================================================
a = license();
asserts.istrue(strcmp(a, 'GNU Lesser General Public License v3.0') || strcmp(a, 'GNU General Public License v3.0'))
%=============================================================================
[a, b] = license();
asserts.istrue(strcmp(a, 'GNU Lesser General Public License v3.0') || strcmp(a, 'GNU General Public License v3.0'))
asserts.istrue(ischar(b));
asserts.istrue(length(b) > 100);
%=============================================================================