%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isquietmode'), 0);
asserts.isequal(nargout('isquietmode'), 1);
%=============================================================================
tf = isquietmode();
asserts.isequal(class(tf), 'logical');
asserts.isequal(size(tf), [1 1]);
%=============================================================================
