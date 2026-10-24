%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isfunction_handle'), 1);
asserts.isequal(nargout('isfunction_handle'), 1);
%=============================================================================
fh = str2func('cos');
asserts.istrue(isfunction_handle(fh));
asserts.istrue(isfunction_handle(@(x) x + 1));
asserts.isfalse(isfunction_handle(3));
asserts.isfalse(isfunction_handle('cos'));
%=============================================================================
