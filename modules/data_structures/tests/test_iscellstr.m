%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('iscellstr'), 1);
asserts.isequal(nargout('iscellstr'), 1);
%=============================================================================
asserts.istrue(iscellstr({'Nelson', 'language'}));
asserts.isfalse(iscellstr({'Nelson', 1}));
asserts.isfalse(iscellstr("Nelson"));
%=============================================================================
asserts.istrue(iscellstr({'ab'; 'cd'}));
asserts.isfalse(iscellstr({'ab'; 1}));
asserts.isfalse(iscellstr("ab"));
%=============================================================================
