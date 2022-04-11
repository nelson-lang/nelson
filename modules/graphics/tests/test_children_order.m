%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
a=figure(1);
b=figure(4);
d=figure(2);
e=figure(3);
c=groot;
child = c.Children;
assert_isequal(child(1).Number, 3);
assert_isequal(child(2).Number, 2);
assert_isequal(child(3).Number, 4);
assert_isequal(child(4).Number, 1);
%=============================================================================
