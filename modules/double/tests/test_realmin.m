%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('realmin'), 1);
asserts.isequal(nargout('realmin'), 1);
%=============================================================================
R = realmin();
asserts.isequal(class(R), 'double');
asserts.istrue(R > 0);
asserts.istrue(R < realmax());
%=============================================================================
R = realmin('double');
asserts.isequal(class(R), 'double');
asserts.istrue(R > 0);
asserts.istrue(R < realmax('double'));
%=============================================================================
R = realmin('single');
asserts.isequal(class(R), 'single');
asserts.istrue(R > 0);
asserts.istrue(R < realmax('single'));
%=============================================================================
