%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Allan", "Yann", "Jack"];
ages = [50, 35, 68]; 
gym = dictionary(names, ages) ;
R = keys(gym);
REF = ["Allan"; "Yann"; "Jack"];
assert_isequal(R, REF);
R = keys(gym, 'cell');
assert_isequal(R, {"Allan"; "Yann"; "Jack"});
%=============================================================================
