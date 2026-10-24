%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('false'), 1);
asserts.isequal(nargout('false'), 1);
%=============================================================================
asserts.isequal(false(), logical(0));
asserts.isequal(false(2, 3), zeros(2, 3, 'logical'));
asserts.isequal(false([1 4]), zeros(1, 4, 'logical'));
%=============================================================================
