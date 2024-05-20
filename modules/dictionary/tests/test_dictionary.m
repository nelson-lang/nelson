%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Jack", "John", "Jess"];
ages = [50, 35, 68]; 
gym = dictionary(names, ages);
assert_isequal(class(gym), 'dictionary');
%=============================================================================
gym2 = dictionary(gym);
assert_isequal(class(gym2), 'dictionary');
%=============================================================================
d = dictionary('a', 1);
assert_istrue(isConfigured(d));
assert_isequal(numEntries(d), 1);
assert_isequal(types(d), "string");
[key_type, val_type] = types(d);
assert_isequal(key_type, "string");
assert_isequal(val_type, "double");
%=============================================================================
