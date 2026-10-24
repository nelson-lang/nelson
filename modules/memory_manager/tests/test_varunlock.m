%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('varunlock'), 2);
asserts.isequal(nargout('varunlock'), 0);
%=============================================================================
unlockValue = 1;
varlock('local', 'unlockValue');
asserts.istrue(varislock('local', 'unlockValue'));
varunlock('local', 'unlockValue');
unlockValue = 2;
asserts.isequal(unlockValue, 2);
asserts.isfalse(varislock('local', 'unlockValue'));
%=============================================================================
