%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isstring'), 1);
asserts.isequal(nargout('isstring'), 1);
%=============================================================================
asserts.istrue(isstring("yeah"));
asserts.isfalse(isstring('yeah'));
asserts.isfalse(isstring(1));
asserts.istrue(isstring(["1", "go"]));
asserts.isfalse(isstring({"1", "go"}));
%=============================================================================
