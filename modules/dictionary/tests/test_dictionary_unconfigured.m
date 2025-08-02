%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = dictionary();
assert_isfalse(isConfigured(d));
assert_isfalse(d.isConfigured);
assert_isequal(length(d), 1);
assert_isequal(numel(d), 1);
assert_isequal(ndims(d), 2);
assert_isequal(size(d), [1 1]);
assert_isequal(numEntries(d), 0);
assert_isequal(d.numEntries, 0);
assert_istrue(ismissing(types(d)));
assert_istrue(ismissing(d.types));
[key_type, val_type] = types(d);
assert_istrue(ismissing(key_type));
assert_istrue(ismissing(val_type));
%=============================================================================
