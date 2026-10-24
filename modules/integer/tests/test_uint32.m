%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('uint32'), 1);
asserts.isequal(nargout('uint32'), 1);
%=============================================================================
R = uint32([-1 0 1]);
asserts.isequal(class(R), 'uint32');
asserts.isequal(R, uint32([0 0 1]));
asserts.isequal(uint32(double(intmax('uint32')) * 2), intmax('uint32'));
asserts.isequal(uint32(-1), intmin('uint32'));
%=============================================================================
