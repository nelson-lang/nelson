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
asserts.isequal(class(s), 'struct');
asserts.isequal(fieldnames(s), {'Type'; 'Seed'; 'State'});
asserts.isequal(size(s), [1 1]);
asserts.isequal(s.Type, 'twister');
asserts.isequal(s.Seed, uint32(0));
asserts.isequal(size(s.State), [625, 1]);
asserts.isequal(class(s.State), 'uint32');
%=============================================================================
R = rng('enginelist');
REF = {'twister';
    'twister64';
    'simdTwister';
    'combRecursive';
    'laggedfibonacci607';
    'philox';
    'threefry';
    'pcg';
    'xoshiro'};
asserts.isequal(R, REF);
%=============================================================================
savedRandomState = rng();
rng('default');
state = rng();
asserts.isequal(class(state), 'struct');
asserts.isequal(fieldnames(state), {'Type'; 'Seed'; 'State'});
rng(savedRandomState);
%=============================================================================
% a string-scalar argument is accepted wherever a character vector is
rng(1, "twister");
a = rand(1, 4);
rng(1, 'twister');
b = rand(1, 4);
asserts.isequal(a, b);
rng("default");
asserts.isequal(rng().Type, 'twister');
rng(7, "philox");
asserts.isequal(rng().Type, 'philox');
rng(savedRandomState);
%=============================================================================
