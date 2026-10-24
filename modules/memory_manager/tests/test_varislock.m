%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('varislock'), 2);
asserts.isequal(nargout('varislock'), 1);
%=============================================================================
lockedValue = 1;
asserts.isfalse(varislock('local', 'lockedValue'));
varlock('local', 'lockedValue');
asserts.istrue(varislock('local', 'lockedValue'));
varunlock('local', 'lockedValue');
asserts.isfalse(varislock('local', 'lockedValue'));
%=============================================================================
