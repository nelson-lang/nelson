%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('uint16'), 1);
asserts.isequal(nargout('uint16'), 1);
%=============================================================================
R = uint16([-1 0 1]);
asserts.isequal(class(R), 'uint16');
asserts.isequal(R, uint16([0 0 1]));
asserts.isequal(uint16(70000), intmax('uint16'));
asserts.isequal(uint16(-1), intmin('uint16'));
%=============================================================================
