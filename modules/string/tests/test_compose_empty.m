%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% An empty numeric argument gives an empty string array (no out-of-bounds read)
R = compose('Row%d', zeros(0, 1));
asserts.isequal(class(R), 'string');
asserts.isequal(size(R), [0 1]);
%=============================================================================
R = compose('Row%d', 1:0);
asserts.isequal(class(R), 'string');
asserts.isequal(size(R), [1 0]);
%=============================================================================
R = compose("Row%d", []);
asserts.isequal(class(R), 'string');
asserts.isequal(size(R), [0 0]);
%=============================================================================
R = compose("%d:%d", zeros(0, 4));
asserts.isequal(class(R), 'string');
asserts.isequal(size(R), [0 2]);
%=============================================================================
R = compose("%s=%d", "x", zeros(0, 1));
asserts.isequal(class(R), 'string');
asserts.isequal(size(R), [0 1]);
%=============================================================================
R = compose('%s', '');
asserts.isequal(R, "");
%=============================================================================
