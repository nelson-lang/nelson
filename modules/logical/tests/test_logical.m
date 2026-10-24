%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('logical'), 1);
asserts.isequal(nargout('logical'), 1);
%=============================================================================
asserts.isequal(logical([0 1 2]), [false true true]);
asserts.isequal(class(logical(1)), 'logical');
%=============================================================================
