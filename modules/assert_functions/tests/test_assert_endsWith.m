%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.endsWith'), 2);
asserts.isequal(nargout('asserts.endsWith'), 2);
%=============================================================================
asserts.endsWith('Nelson language', 'language');
%=============================================================================
[r, msg] = asserts.endsWith('Nelson language', 'Nelson');
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.endsWith'));
%=============================================================================
