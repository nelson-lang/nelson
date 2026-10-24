%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.matchesAny'), 2);
asserts.isequal(nargout('asserts.matchesAny'), 2);
%=============================================================================
asserts.matchesAny('abc123', {'^xyz', '[0-9]+$'});
%=============================================================================
[r, msg] = asserts.matchesAny('abc123', {'^xyz', 'zzz'});
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.matchesAny'));
%=============================================================================
