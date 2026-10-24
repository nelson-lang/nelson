%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isobject'), 1);
asserts.isequal(nargout('isobject'), 1);
%=============================================================================
A = 1;
res = isobject(A);
asserts.isfalse(res);
%=============================================================================
addpath([modulepath('overload', 'root'), '/examples/complex']);
A = complexObj(1, 2);
res = isobject(A);
asserts.istrue(res);
%=============================================================================
