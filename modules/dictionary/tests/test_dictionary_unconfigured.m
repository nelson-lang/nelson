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
asserts.isfalse(isConfigured(d));
asserts.isfalse(d.isConfigured);
asserts.isequal(length(d), 1);
asserts.isequal(numel(d), 1);
asserts.isequal(ndims(d), 2);
asserts.isequal(size(d), [1 1]);
asserts.isequal(numEntries(d), 0);
asserts.isequal(d.numEntries, 0);
asserts.istrue(ismissing(types(d)));
asserts.istrue(ismissing(d.types));
[key_type, val_type] = types(d);
asserts.istrue(ismissing(key_type));
asserts.istrue(ismissing(val_type));
%=============================================================================
