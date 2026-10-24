%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '.json'];
filewrite(filename, '{"a": 1, "b": 2}');
%=============================================================================
d = readdictionary(filename);
asserts.isequal(class(d), 'dictionary');
asserts.isequal(keys(d), ["a"; "b"]);
asserts.isequal(values(d), [1; 2]);
asserts.isequal(numEntries(d), 2);
%=============================================================================
rmfile(filename);
%=============================================================================
