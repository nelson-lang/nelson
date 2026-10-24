%=============================================================================
% Copyright (c) 2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('ismodule'), 1);
asserts.isequal(nargout('ismodule'), 1);
%=============================================================================
asserts.istrue(ismodule('core'));
asserts.istrue(ismodule('core', 'isprotected'));
%=============================================================================
asserts.isfalse(ismodule('foo'));
asserts.isfalse(ismodule('foo', 'isprotected'));
%=============================================================================
