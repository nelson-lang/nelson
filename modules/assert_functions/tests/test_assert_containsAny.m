%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.containsAny'), 2);
asserts.isequal(nargout('asserts.containsAny'), 2);
%=============================================================================
asserts.containsAny('Nelson language', {'toolbox', 'Nelson'});
%=============================================================================
[r, msg] = asserts.containsAny('Nelson language', {'toolbox', 'module'});
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.containsAny'));
%=============================================================================
