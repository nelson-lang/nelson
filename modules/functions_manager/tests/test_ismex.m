%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('ismex'), 1);
asserts.isequal(nargout('ismex'), 1);
%=============================================================================
asserts.isfalse(ismex('cos'));
asserts.isfalse(ismex('nelson_missing_function_for_test'));
asserts.isfalse(ismex("cos"));
%=============================================================================
