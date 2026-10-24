%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.containsAll'), 2);
asserts.isequal(nargout('asserts.containsAll'), 2);
%=============================================================================
asserts.containsAll('Nelson language', {'Nelson', 'language'});
%=============================================================================
[r, msg] = asserts.containsAll('Nelson language', {'Nelson', 'toolbox'});
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.containsAll'));
%=============================================================================
