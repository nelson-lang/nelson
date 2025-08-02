%=============================================================================
% Copyright (c) 2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('ismodule'), 1);
assert_isequal(nargout('ismodule'), 1);
%=============================================================================
assert_istrue(ismodule('core'));
assert_istrue(ismodule('core', 'isprotected'));
%=============================================================================
assert_isfalse(ismodule('foo'));
assert_isfalse(ismodule('foo', 'isprotected'));
%=============================================================================
