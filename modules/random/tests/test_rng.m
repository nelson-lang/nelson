%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
rng('default');
s = rng();
assert_isequal(class(s), 'struct');
assert_isequal(fieldnames(s), {'Type'; 'Seed'; 'State'});
assert_isequal(size(s), [1 1]);
assert_isequal(s.Type, 'twister');
assert_isequal(s.Seed, uint32(0));
assert_isequal(size(s.State), [625, 1]);
assert_isequal(class(s.State), 'uint32');
%=============================================================================
R = rng('enginelist');
REF = {'twister';           
    'twister64';         
    'simdTwister';       
    'combRecursive';     
    'laggedfibonacci607';
    'philox';
    'threefry'};
assert_isequal(R, REF);
%=============================================================================
