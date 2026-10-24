%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('double'), 1);
asserts.isequal(nargout('double'), 1);
%=============================================================================
asserts.isequal(class(double(single(3))), 'double');
asserts.isequal(double(uint8([65 66])), [65 66]);
asserts.isequal(double('ABC'), [65 66 67]);
%=============================================================================
