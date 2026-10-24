%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.hasFields'), 2);
asserts.isequal(nargout('asserts.hasFields'), 2);
%=============================================================================
S = struct('a', 1, 'b', 2);
asserts.hasFields(S, {'a', 'b'});
%=============================================================================
[r, msg] = asserts.hasFields(S, {'a', 'c'});
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.hasFields'));
%=============================================================================
