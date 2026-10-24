%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isclass'), 1);
asserts.isequal(nargout('isclass'), 1);
%=============================================================================
A = 3;
res = isclass(A);
asserts.isfalse(res);
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
c = complexObj(3, 4);
res = isclass(c);
asserts.istrue(res);
%=============================================================================
