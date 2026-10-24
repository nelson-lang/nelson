%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isscalar'), 1);
asserts.isequal(nargout('isscalar'), 1);
%=============================================================================
asserts.istrue(isscalar(42));
asserts.isfalse(isscalar([1 2]));
asserts.isfalse(isscalar([]));
%=============================================================================
