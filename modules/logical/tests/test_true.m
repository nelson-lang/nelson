%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('true'), 1);
asserts.isequal(nargout('true'), 1);
%=============================================================================
asserts.isequal(true(), logical(1));
asserts.isequal(true(2, 3), ones(2, 3, 'logical'));
asserts.isequal(true([1 4]), ones(1, 4, 'logical'));
%=============================================================================
