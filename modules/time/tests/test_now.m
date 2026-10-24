%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('now'), 0);
asserts.isequal(nargout('now'), 1);
n = now();
asserts.isequal(class(n), 'double');
asserts.istrue(n > datenum(2000, 1, 1));
%=============================================================================
