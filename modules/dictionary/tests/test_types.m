%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Jack", "John", "Jess"];
ages = [50, 35, 68]; 
gym = dictionary(names, ages);
%=============================================================================
[keyType, valueType] = types(gym);
assert_isequal(keyType, "string");
assert_isequal(valueType, "double");
%=============================================================================
names = {'Jack', 'John', 'Jess'};
sz = {'S', 'XL', 'XXL'}; 
gym = dictionary(names, sz);
[keyType, valueType] = types(gym);
assert_isequal(keyType, "cell");
assert_isequal(valueType, "cell");
%=============================================================================
